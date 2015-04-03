
-- | Directory utils that don't need to be in the Build monad.
module BuildBox.IO.Directory
        ( lsFilesIn
        , lsDirsIn
        , traceFilesFrom)
where
import System.Directory
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Sequence            (Seq)
import qualified Data.Sequence  as Seq


-- | Get the names of all files in a directory.
--   This filters out the fake files like '.' and '..'
lsFilesIn :: MonadIO m => String -> m [String]
lsFilesIn path
 = do   contents <- liftIO $ getDirectoryContents path
        
        -- filter out directories
        files   <- filterM (\p -> liftM not $ liftIO $ doesDirectoryExist p) 
                $ map (\f -> path ++ "/" ++ f)
                $ dropDotPaths contents

        return  $ sort files


-- | Get the names of all the dirs in this one.
--   This filters out the fake files like '.' and '..'
lsDirsIn :: MonadIO m => String  -> m [String]
lsDirsIn path
 = do 
        contents <- liftIO $ getDirectoryContents path
        
        -- only keep directories
        dirs    <- filterM (liftIO . doesDirectoryExist)
                $  map (\f -> path ++ "/" ++ f)
                $  dropDotPaths contents

        return  $ sort dirs


-- | Get all the files reachable from this directory
traceFilesFrom :: FilePath -> IO (Seq FilePath)
traceFilesFrom path
 = do   isDir   <- doesDirectoryExist path
        isFile  <- doesFileExist      path

        let result
                | isDir         
                = do    contents <- liftM dropDotPaths
                                 $  getDirectoryContents path

                        liftM (join  . Seq.fromList)
                                $ mapM traceFilesFrom 
                                $ map (\f -> path ++ "/" ++ f) 
                                $ contents

                | isFile
                =       return  $ Seq.singleton path
                
                | otherwise
                =       return  $ Seq.empty
        
        result


-- | Drop out the fake '.' and '..' dirst from a list of paths.
dropDotPaths :: [FilePath] -> [FilePath]
dropDotPaths xx
        = filter (\f -> f /= "." && f /= "..") xx
