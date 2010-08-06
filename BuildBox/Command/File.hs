
-- | Preconditions that we can check for explicitly.
module BuildBox.Command.File
	(PropFile(..))
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
		
	