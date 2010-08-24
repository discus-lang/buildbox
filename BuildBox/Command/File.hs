
-- | Working with the file system.
module BuildBox.Command.File
	( PropFile(..)
	, makeDirIfNeeded
	, inDir
	, inNewScratchDirNamed
	, clobberDir
	, withTempFile)
where
import BuildBox.Build.Base
import BuildBox.Build.Testable
import BuildBox.Command.System
import System.Posix.Temp
import System.Directory
import System.IO

-- | Properties of the file system we can test for.
data PropFile

	-- | Some executable is in the current path.
	= HasExecutable	String

	-- | Some file exists.
	| HasFile	FilePath

	-- | Some directory exists.
	| HasDir  	FilePath

	-- | Some file is empty.
	| FileEmpty 	FilePath
	deriving Show


instance Testable PropFile where
 test prop 
  = case prop of
	HasExecutable name
	 -> do	code	<- qsystem $ "which " ++ name
		return	$ code == ExitSuccess

	HasFile path	
	 -> io $ doesFileExist path

	HasDir  path
	 -> io $ doesDirectoryExist path

	FileEmpty  path 
	 -> do	contents	<- io $ readFile path
		return (null contents)


-- Directories ------------------------------------------------------------------------------------
-- | Create a new directory if it isn't already there, or return successfully if it is.
makeDirIfNeeded :: FilePath -> Build ()
makeDirIfNeeded path
 = do	already	<- io $ doesDirectoryExist path
	if already
	 then return ()
	 else ssystem $ "mkdir -p " ++ path


-- | Run a command in a different working directory. Throws an error if the directory doesn't exist.
inDir :: FilePath -> Build a -> Build a
inDir name build
 = do	check $ HasDir name
	oldDir	<- io $ getCurrentDirectory

	io $ setCurrentDirectory name
	x	<- build
	io $ setCurrentDirectory oldDir

	return x


-- | Delete a dir recursively if it's there, otherwise do nothing.
--   This behaves differently than `removeDirectoryRecursive` because it does
--   not follow symlinks, just deletes them.
clobberDir :: FilePath -> Build ()
clobberDir path
 = 	ssystem $ "rm -Rf " ++ path


-- Scratch ----------------------------------------------------------------------------------------
-- | Create a new directory with the given name, run a command within it,
--   then change out and recursively delete the directory. Throws an error if a directory
--   with the given name already exists.
inNewScratchDirNamed :: FilePath -> Build a -> Build a
inNewScratchDirNamed name build
 = do	
	-- Make sure a dir with this name doesn't already exist.
	checkFalse $ HasDir name

	ssystem $ "mkdir -p " ++ name
	x	<- inDir name build
	
	ssystem $ "rm -Rf " ++ name 
	return x


-- | Create a temp file, pass it to some command, then delete the file after the command finishes.
withTempFile :: (FilePath -> Build a) -> Build a
withTempFile build
 = do	(fileName, handle)	<- io $ mkstemp "/tmp/buildbox-XXXXXX"

	-- We just want the file name here, so close the handle to let the real
	-- build command write to it however it wants.
	io $ hClose handle
	
	-- run the real command
	result	<- build fileName
	
	-- cleanup 
	io $ removeFile fileName
	
	return result
	
	




