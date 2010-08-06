
-- | Preconditions that we can check for explicitly.
module BuildBox.Build.Property
	(Property(..))
where
import BuildBox.Build.Base
import BuildBox.Build.Testable
import System.Directory

-- | Preconditions that we know about.
data Property
	= FileExists FilePath
	| DirExists  FilePath
	| FileEmpty  FilePath
	deriving Show

instance Testable Property where
 test prop 
  = case prop of
	FileExists path	
	 -> io $ doesFileExist path

	DirExists  path
	 -> io $ doesDirectoryExist path

	FileEmpty  path 
	 -> do	contents	<- io $ readFile path
		return (null contents)
		
	