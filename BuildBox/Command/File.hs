
-- | Preconditions that we can check for explicitly.
module BuildBox.Command.File
	( PropFile(..)
	, inDir
	, inNewScratchDirNamed)
where
import BuildBox.Build.Base
import BuildBox.Build.Testable
import BuildBox.Command.System
import System.Directory

-- | Properties of the file system we can check.
data PropFile

	-- | Use which to check if we have an executable.
	= HasExecutable	String

	-- | Check if a file exists.
	| HasFile	FilePath

	-- | Check if a directory exists.
	| HasDir  	FilePath

	-- | Check if a file empty.
	| FileEmpty 	FilePath
	deriving Show


instance Testable PropFile where
 test prop 
  = case prop of
	HasExecutable name
	 -> do	code	<- systemNullCode $ "which " ++ name
		return	$ code == ExitSuccess

	HasFile path	
	 -> io $ doesFileExist path

	HasDir  path
	 -> io $ doesDirectoryExist path

	FileEmpty  path 
	 -> do	contents	<- io $ readFile path
		return (null contents)


-- Working Directories ----------------------------------------------------------------------------
inDir :: FilePath -> Build a -> Build a
inDir name build
 = do	check $ HasDir name
	oldDir	<- io $ getCurrentDirectory

	io $ setCurrentDirectory name
	x	<- build
	io $ setCurrentDirectory oldDir

	return x

-- Scratch ----------------------------------------------------------------------------------------
-- | Create a new dir with this path, change into it, run a build, change out, then delete the dir.
inNewScratchDirNamed :: FilePath -> Build a -> Build a
inNewScratchDirNamed name build
 = do	
	-- Make sure a dir with this name doesn't already exist.
	checkNot $ HasDir name

	system $ "mkdir -p " ++ name
	x	<- inDir name build
	
	system $ "rm -Rf " ++ name 
	return x

