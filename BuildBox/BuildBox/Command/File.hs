
-- | Working with the file system.
module BuildBox.Command.File
	( PropFile(..)
	, inDir
	, inScratchDir
	, clobberDir
	, ensureDir
	, withTempFile)
where
import BuildBox.Build
import BuildBox.Command.System
import System.Directory
import Control.Monad.State

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


-- | Run a command in a different working directory. Throws an error if the directory doesn't exist.
inDir :: FilePath -> Build a -> Build a
inDir name build
 = do	check $ HasDir name
	oldDir	<- io $ getCurrentDirectory

	io $ setCurrentDirectory name
	x	<- build
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

	ssystem $ "mkdir -p " ++ name
	x	<- inDir name build
	
	ssystem $ "rm -Rf " ++ name 
	return x


-- | Delete a dir recursively if it's there, otherwise do nothing.
--   Unlike `removeDirectoryRecursive`, this function does
--   not follow symlinks, it just deletes them.
clobberDir :: FilePath -> Build ()
clobberDir path
 = 	ssystem $ "rm -Rf " ++ path


-- | Create a new directory if it isn't already there, or return successfully if it is.
ensureDir :: FilePath -> Build ()
ensureDir path
 = do	already	<- io $ doesDirectoryExist path
	if already
	 then return ()
	 else ssystem $ "mkdir -p " ++ path


-- | Create a temp file, pass it to some command, then delete the file after the command finishes.
withTempFile :: (FilePath -> Build a) -> Build a
withTempFile build
 = do	fileName	<- newTempFile

	-- run the real command
	result	<- build fileName
	
	-- cleanup 
	io $ removeFile fileName
	
	return result
	

-- | Allocate a new temporary file name
newTempFile :: Build FilePath
newTempFile 
 = do	buildDir	<- gets buildStateScratchDir
	buildId		<- gets buildStateId 
	buildSeq	<- gets buildStateSeq 
	
	-- Increment the sequence number.
	modify $ \s -> s { buildStateSeq = buildStateSeq s + 1 }
	
	-- Build the file name we'll try to use.
	fileName	<- io $ canonicalizePath 
			$  buildDir ++ "/buildbox-" ++ show buildId ++ "-" ++ show buildSeq
	
	-- If it already exists then something has gone badly wrong.
	--   Maybe the unique Id for the process wasn't as unique as we thought.
	exists		<- io $ doesFileExist fileName
	when exists
	 $ error "buildbox: panic, supposedly fresh file already exists."
	
	-- Touch the file for good measure.
	--   If the unique id wasn't then we want to detect this.
	io $ writeFile fileName ""
	
	return fileName





