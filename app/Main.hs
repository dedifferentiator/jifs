module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.Directory
import System.Posix.Directory.Foreign
import System.Posix.Directory.Traversals
import System.Posix.ByteString.FilePath
import System.Posix.FilePath ((</>))
import System.Posix.IO
import GHC.IO.Handle
import System.Fuse
import System.IO
import Control.Exception (bracket)


type HT = ()

main :: IO ()
main = fuseMain helloFSOps defaultExceptionHandler

helloFSOps :: FuseOperations Handle
helloFSOps = defaultFuseOps { fuseGetFileStat        = nGetFileStat
                            , fuseOpen               = nOpen
                            , fuseRelease            = nRelease
                            , fuseRead               = nRead
                            , fuseWrite              = nWrite
                            , fuseOpenDirectory      = nOpenDirectory
                            , fuseReadDirectory      = nReadDirectory
                            , fuseGetFileSystemStats = helloGetFileSystemStats
                            }

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/hello"

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat :: FuseContext -> FileStat
fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = 0  -- replace with real value
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

helloGetFileStat :: FilePath -> IO (Either Errno FileStat)
helloGetFileStat "/" = do
    Right . dirStat <$> getFuseContext
helloGetFileStat path | path == helloPath = do
    Right . fileStat <$> getFuseContext
helloGetFileStat _ =
    return $ Left eNOENT

nGetFileStat :: FilePath -> IO (Either Errno FileStat)
nGetFileStat p = do
    st <- getFileStatus p
    return $ Right $ FileStat
                     (fileModeToEntryType $ fileMode st)
                     (fileMode st)
                     (linkCount st)
                     (fileOwner st)
                     (fileGroup st)
                     (specialDeviceID st)
                     (fileSize st)
                     1
                     (accessTime st)
                     (modificationTime st)
                     (statusChangeTime st)


nOpenDirectory :: String -> IO Errno
nOpenDirectory "/" = return eOK
nOpenDirectory _   = return eNOENT

helloReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(helloName,    fileStat ctx)
                   ]
    where (_:helloName) = helloPath
helloReadDirectory _ = return (Left eNOENT)

defaultStats :: FuseContext -> [(FilePath, FileStat)]
defaultStats ctx = [(".", dirStat ctx), ("..", dirStat ctx)]


getDirStat :: FuseContext -> IO FileStat
getDirStat ctx = return $ dirStat ctx

nReadDirectoryFake :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
nReadDirectoryFake _ = Right . defaultStats <$> getFuseContext

toFileStat :: FileStatus -> FileStat
toFileStat st = FileStat (fileModeToEntryType $ fileMode st)
                          (fileMode st)
                          (linkCount st)
                          (fileOwner st)
                          (fileGroup st)
                          (specialDeviceID st)
                          (fileSize st)
                          1
                          (accessTime st)
                          (modificationTime st)
                          (statusChangeTime st)


-- reads dir content from dir stream
readDirCon :: DirStream -> IO (Either Errno [(FilePath, FileStat)])
readDirCon ds = do
    (_, p) <- readDirEnt ds
    if B.null p
    then return $ Right []
    else (<>) . fstat p <$> getFileStatus (toPath p) <*> readDirCon ds

        where fstat p st = Right [(toPath p, toFileStat st)]
              toPath p = B.unpack $ B.tail p



readDirConFake :: DirStream -> IO (Either Errno [(FilePath, FileStat)])
readDirConFake _ = helloReadDirectory "/"

-- reads direcotry content from path
-- FIXME: doesn't support reading from other dirs except root, ((why?))
nReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
nReadDirectory "/" = -- (Right . defaultStats <$> getFuseContext) <>
    bracket
    (openDirStream "/")
    closeDirStream
    readDirCon
nReadDirectory _ = return $ Left eNOENT -- jifs doesn't support directories yet

helloOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
helloOpen path mode _
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


toIOMode :: OpenMode -> OpenFileFlags -> IOMode
toIOMode ReadOnly _ = ReadMode
toIOMode WriteOnly (OpenFileFlags a _ _ _ _) = if a then AppendMode else WriteMode
toIOMode ReadWrite _ = ReadWriteMode


nOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Handle)
nOpen p mode flags = Right <$> openFile p (toIOMode mode flags)


nRead :: FilePath -> Handle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
nRead _ h count offset = do
    hSeek h AbsoluteSeek (toInteger offset)
    Right <$> B.hGet h (fromInteger $ toInteger count)


nRelease :: FilePath -> Handle -> IO ()
nRelease _ = hClose


nWrite :: FilePath -> Handle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
nWrite _ h content offset = do
    hSeek h AbsoluteSeek (toInteger offset)
    B.hPut h content
    return $ Right . fromInteger . toInteger $ B.length content

-- tagfsCreateLink :: FilePath -> FilePath -> IO Errno
-- tagfsCreateLink src dst = do
-- 	let dstpath = dropFileName dst
-- 	forFile r src (return eNOENT) (const $ return eINVAL) $ \f -> case f of
-- 		RegularFile name ->
-- 			forDir r dstpath (return eNOENT) (const $ return eNOTDIR) $ \d -> case d of
-- 				TagDir tag -> do
-- 					let ts = getTagSet status
-- 					let tsNew = addTag tag name ts
-- 					updateStatusRef ref tsNew
-- 					return eOK
-- 				_ -> return eINVAL
-- 		_ -> return eINVAL


helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats _ =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
