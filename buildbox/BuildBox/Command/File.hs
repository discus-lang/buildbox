
-- | Working with the file system.
module BuildBox.Command.File
        ( PropFile(..)
        , inDir
        , inScratchDir
        , clobberDir
        , ensureDir
        , withTempFile
        , atomicWriteFile
        , exe )
where
import BuildBox.Build
import System.Directory
-- import Control.Exception
import Control.Monad.State
import Control.Monad.Catch
import System.Info
import qualified System.IO.Temp         as System
import qualified System.IO              as System

-- | Properties of the file system we can test for.
data PropFile

        -- | Some executable is in the current path.
        = HasExecutable String

        -- | Some file exists.
        | HasFile       FilePath

        -- | Some directory exists.
        | HasDir        FilePath

        -- | Some file is empty.
        | FileEmpty     FilePath
        deriving Show


instance Testable PropFile where
 test prop
  = case prop of
        HasExecutable name
         -> do  bin <- io $ findExecutable name
                return $ case bin of
                 Just _         -> True
                 Nothing        -> False

        HasFile path
         -> io $ doesFileExist path

        HasDir  path
         -> io $ doesDirectoryExist path

        FileEmpty  path
         -> do  contents        <- io $ readFile path
                return (null contents)


-- | Run a command in a different working directory. Throws an error if the directory doesn't exist.
inDir :: FilePath -> Build a -> Build a
inDir name build
 = do   check $ HasDir name
        oldDir  <- io $ getCurrentDirectory

        io $ setCurrentDirectory name
        x       <- build
        io $ setCurrentDirectory oldDir

        return x

-- | Create a new directory with the given name, run a command within it,
--   then change out and recursively delete the directory. Throws an error if a directory
--   with the given name already exists.
inScratchDir :: FilePath -> Build a -> Build a
inScratchDir name build
 = do
        -- Make sure a dir with this name doesn't already exist.
        checkFalse $ HasDir name

        ensureDir name
        x       <- inDir name build
        clobberDir name

        return x


-- | Delete a dir recursively if it's there, otherwise do nothing.
clobberDir :: FilePath -> Build ()
clobberDir path
 = do   e <- io $ try $ removeDirectoryRecursive path
        case (e :: Either SomeException ()) of
         _      -> return ()


-- | Create a new directory if it isn't already there, or return successfully if it is.
ensureDir :: FilePath -> Build ()
ensureDir path
 = do   already <- io $ doesDirectoryExist path
        if already
         then return ()
         else do e <- io $ try $ createDirectoryIfMissing True path
                 case (e :: Either SomeException ()) of
                  _     -> return ()


-- | Create a temp file, pass it to some command, then delete the file after the command finishes.
withTempFile :: (FilePath -> Build a) -> Build a
withTempFile build
 = do   buildDir        <- gets buildStateScratchDir
        buildSeq        <- gets buildStateSeq

        -- File name template.
        let sTemplate   = "buildbox-" ++ show buildSeq ++ ".tmp"

        System.withTempFile 
                buildDir sTemplate
                (\  fileName h 
                 -> do  io $ System.hClose h
                        build fileName)


-- | Atomically write a file by first writing it to a tmp file then renaming it.
--   This prevents concurrent processes from reading half-written files.
atomicWriteFile :: FilePath -> String -> Build ()
atomicWriteFile filePath str
 = do   buildDir        <- gets buildStateScratchDir
        buildSeq        <- gets buildStateSeq

        -- File name template.
        let sTemplate   = "buildbox-" ++ show buildSeq ++ ".tmp"

        -- Create a new temp file.
        (tmp, h)        <- io $ System.openBinaryTempFile buildDir sTemplate
        io $ System.hClose h

        -- Write the data to the temp file.
        io $ writeFile tmp str
        e <- io $ try $ renameFile tmp filePath

        -- renameFile may not be able to rename files across physical devices, 
        -- depending on the implementation. If renameFile fails then try copyFile.
        case (e :: Either SomeException ()) of
         Right _           
          -> return ()

         Left _
          -> do io $ copyFile tmp filePath
                io $ removeFile tmp
                return ()


-- | The file extension for an executable on the current system.
exe :: String
exe
 | os == "mingw32"      = "exe"
 | otherwise            = ""
