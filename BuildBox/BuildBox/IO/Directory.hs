
-- | Directory utils that don't need to be in the Build monad.
module BuildBox.IO.Directory
	(traceFilesFrom)
where
import System.Directory
import Data.Sequence		(Seq)
import qualified Data.Foldable	as Seq
import qualified Data.Sequence	as Seq


-- | Get all the files reachable from this directory
traceFilesFrom :: FilePath -> IO (Seq FilePath)
traceFilesFrom path
 = do	isDir	<- doesDirectoryExist path
	isFile	<- doesFileExist      path

	let result
		| isDir 	
		= do	contents <- liftM (filter (\s -> not $ elem s [".", ".."]))
			 	 $  getDirectoryContents path

			liftM (join  . Seq.fromList)
				$ mapM traceFilesFrom 
				$ map (\f -> path ++ "/" ++ f) 
				$ contents

		| isFile
		=	return	$ Seq.singleton path
		
		| otherwise
		=	return	$ Seq.empty
	
	result

module 