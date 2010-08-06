
-- | Preconditions that we can check for explicitly.
module BuildBox.Command.File
	(Property(..))
where
import BuildBox.Build.Base
import BuildBox.Build.Testable
import System.Directory

-- | Preconditions that we know about.
data PropFile
	= FileExists FilePath
	| DirExists  FilePath
	| FileEmpty  FilePath
	deriving Show

instance Testable PropFile where
 test prop 
  = case prop of
	FileExists path	
	 -> io $ doesFileExist path

	DirExists  path
	 -> io $ doesDirectoryExist path

	FileEmpty  path 
	 -> do	contents	<- io $ readFile path
		return (null contents)
		
	