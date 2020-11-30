-- Copyright (C) 2020 yohashi
--               2020 dedifferentiator

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Main where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Function (fix)
import Data.Void (Void)
import Foreign.C.Types (CInt(..))
import Foreign.C.Error (Errno(..), eACCES, eOK, eOPNOTSUPP)
import System.Posix.Types (Fd(Fd), COff(..), ByteCount, DeviceID, FileMode, FileOffset)
import qualified System.Posix.Files as F
import System.Posix.Directory
    (createDirectory,
     openDirStream,
     readDirStream,
     removeDirectory,
     closeDirStream)
import System.Posix.IO
    (OpenFileFlags(..),
     OpenMode(..),
     openFd,
     closeFd,
     defaultFileFlags,
     handleToFd)
import System.LibFuse3
import System.Linux.XAttr (lCreateXAttr, lGetXAttr, lReplaceXAttr, lSetXAttr)
import System.IO
    (Handle,
     hClose,
     hFlush,
     hSeek,
     openFile,
     stderr,
     hPutStr,
     SeekMode(AbsoluteSeek),
     IOMode(..))


main :: IO ()
main = fuseMain nFSOps defaultExceptionHandler

nFSOps :: FuseOperations Handle Void
nFSOps = defaultFuseOperations
  { fuseGetattr            = Just nGetAttr
  , fuseOpen               = Just nOpen
  , fuseRelease            = Just nRelease
  , fuseFlush              = Just nFlush
  , fuseRead               = Just nRead
  , fuseWrite              = Just nWrite
  , fuseReaddir            = Just $ \path _ -> nReadDirectory path
  , fuseMkdir              = Just nMkdir
  , fuseMknod              = Just nMknod
  , fuseRmdir              = Just nRmdir
  , fuseReadlink           = Just nReadLink
  , fuseLink               = Just nCreateLink
  , fuseSymlink            = Just nSymLink
  , fuseUnlink             = Just nUnlink
  , fuseStatfs             = Just nStatFS
  , fuseFallocate          = Just $ const nFallocate
  , fuseSetxattr           = Just nSetxattr
  , fuseGetxattr           = Just nGetxattr
  }

foreign import ccall "posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt

-- implements (@posix_fallocate(3)@)
nFallocate :: Handle -> CInt -> FileOffset -> FileOffset -> IO Errno
nFallocate _ mode _ _ | mode /= 0 = pure eOPNOTSUPP
nFallocate h _ offset len = do
  (Fd fd) <- handleToFd h
  Errno <$> c_posix_fallocate fd offset len

-- implements @setxattr(2)@
nSetxattr :: FilePath -> String -> B.ByteString -> SetxattrFlag -> IO Errno
nSetxattr path name value flag =
  let f = case flag of
            SetxattrDefault -> lSetXAttr
            SetxattrCreate  -> lCreateXAttr
            SetxattrReplace -> lReplaceXAttr
  in tryErrno_ $ f path name value

-- implements @getxattr(2)@.
nGetxattr :: FilePath -> String -> IO (Either Errno B.ByteString)
nGetxattr path name = tryErrno $ lGetXAttr path name

-- implements (POSIX @lstat(2)@)
nGetAttr :: FilePath -> Maybe Handle -> IO (Either Errno FileStat)
nGetAttr path _ = tryErrno . getFileStat $ path

-- implements (POSIX @mkdir(2)@)
nMkdir :: FilePath -> FileMode -> IO Errno
nMkdir path mode = tryErrno_ $ createDirectory path mode

-- implements (POSIX @mknod(2)@)
nMknod :: FilePath -> FileMode -> DeviceID -> IO Errno
nMknod path mode rdev = tryErrno_ $ case fileModeToEntryType mode of
  RegularFile -> bracket
                 (openFd path WriteOnly (Just mode) (defaultFileFlags{exclusive=True}))
                 closeFd
                 (\_ -> pure ())
  Directory -> createDirectory path mode
  NamedPipe -> F.createNamedPipe path mode
  _ -> F.createDevice path mode rdev

-- implements (POSIX @rmdir(2)@)
nRmdir :: FilePath -> IO Errno
nRmdir = tryErrno_ . removeDirectory

-- implements @readdir(3)@
nReadDirectory :: FilePath -> IO (Either Errno [(FilePath, Maybe FileStat)])
nReadDirectory path = tryErrno
  $ bracket (openDirStream path) closeDirStream
  $ \dp -> fmap reverse $ flip fix []
  $ \loop acc -> do
    entry <- readDirStream dp
    if null entry
      then pure acc
      else loop $ (entry, Nothing) : acc

-- merges OpenMode and OpenFileFlags to IOMode
toIOMode :: OpenMode -> OpenFileFlags -> IOMode
toIOMode ReadOnly _ = ReadMode
toIOMode WriteOnly (OpenFileFlags a _ _ _ _) = if a then AppendMode else WriteMode
toIOMode ReadWrite _ = ReadWriteMode

-- outputs to stdout mode and file control flags
nDebugPrintMode :: FilePath
                -> OpenMode
                -> OpenFileFlags
                -> IO (Either Errno Handle)
                -> IO (Either Errno Handle)
nDebugPrintMode path mode flags hFD = do
  putStrLn $ "Path -> " <> path 
          <> "\nOpenFileFlags ->\nappend: " <> show (append flags)
          <> "\n\texclusive: "              <> show (exclusive flags)
          <> "\n\tnoctty: "                 <> show (noctty flags)
          <> "\n\tnonBlock: "               <> show (nonBlock flags)
          <> "\n\ttrunc: "                  <> show (trunc flags)
          <> "\n\nMode -> \n\t" <> case mode of
                                  ReadOnly  -> "ReadOnly"
                                  WriteOnly -> "WriteOnly"
                                  ReadWrite -> "ReadWrite"
          <> "\n"
  hFD

-- implements (POSIX @open(2)@)
nOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Handle)
nOpen path mode flags = nDebugPrintMode path mode flags $
  Right <$> openFile path (toIOMode mode flags)

-- implements  Unix98 @pread(2)@
nRead :: FilePath -> Handle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
nRead _ h count offset = do
  hSeek h AbsoluteSeek (toInteger offset)
  Right <$> B.hGet h (fromInteger $ toInteger count)

-- closes handle, calling on already closed handle has no effect
nRelease :: FilePath -> Handle -> IO ()
nRelease _ = hClose

-- forces all buffered for the output items to be sent to the OS
nFlush :: FilePath -> Handle -> IO Errno
nFlush _ h = catch
  (hFlush h >>= (\_ -> return eOK))
  (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr $ "Cannot flush file: " <> err
      return eACCES)

-- implements Unix98 @pwrite(2)@
nWrite :: FilePath -> Handle -> B.ByteString -> FileOffset -> IO (Either Errno CInt)
nWrite _ h content offset = do
  hSeek h AbsoluteSeek (toInteger offset)
  B.hPut h content
  return $ Right . fromInteger . toInteger $ B.length content

nReadLink :: FilePath -> IO (Either Errno FilePath)
nReadLink = tryErrno . F.readSymbolicLink

--implements (POSIX @link(2)@).
nCreateLink :: FilePath -> FilePath -> IO Errno
nCreateLink src dst = tryErrno_ $ F.createLink src dst

-- implements (POSIX @unlink(2)@)
nSymLink :: FilePath -> FilePath -> IO Errno
nSymLink src dst = tryErrno_ $ F.createSymbolicLink src dst

-- implements (POSIX @unlink(2)@)
nUnlink :: FilePath -> IO Errno
nUnlink = tryErrno_ . F.removeLink

-- implements @statfs(2)@.
nStatFS :: FilePath -> IO (Either Errno FileSystemStats)
nStatFS = tryErrno . getFileSystemStats