{-# OPTIONS_HADDOCK hide #-}

module BuildBox.Build.Base where
import BuildBox.Pretty
import BuildBox.Build.BuildError
import BuildBox.Build.BuildState
import Control.Monad.Error
import Control.Monad.State
import System.IO
import System.IO.Error
import System.Random
import System.Directory

-- | The builder monad encapsulates and IO action that can fail with an error, 
--   and also read some global configuration info.
type Build a 	= ErrorT BuildError (StateT BuildState IO) a


-- Build ------------------------------------------------------------------------------------------
-- | Run a build command. The first argument is a directory that can be used for
--   temporary files (like \"/tmp\")
runBuild :: FilePath -> Build a -> IO (Either BuildError a)
runBuild scratchDir build
 = do	uid	<- getUniqueId
	let s	= buildStateDefault uid scratchDir
	evalStateT (runErrorT build) s


-- | Like 'runBuild`, but Run a build command, reporting whether it succeeded to the console.
--   If it succeeded then return Just the result, else Nothing.
runBuildPrint :: FilePath -> Build a -> IO (Maybe a)
runBuildPrint scratchDir build
 = do	uid	<- getUniqueId
	let s	= buildStateDefault uid scratchDir
	runBuildPrintWithState s build


-- | Like `runBuildPrintWithConfig` but also takes a `BuildConfig`.
runBuildPrintWithState :: BuildState -> Build a -> IO (Maybe a)
runBuildPrintWithState s build
 = do	result	<- evalStateT (runErrorT build) s
	case result of
	 Left err
	  -> do	putStrLn "\nBuild failed"
		putStr   "  due to "
		putStrLn $ render $ ppr err
		return $ Nothing
		
	 Right x
	  -> do	putStrLn "Build succeeded."
		return $ Just x


-- | Discard the resulting value of a compuation.
--   Used like @successfully . runBuild ...@
successfully :: IO a -> IO ()
successfully f 	= f >> return ()


-- | Get a unique(ish) id for this process.
--   The random seeds the global generator with the cpu time in psecs, which should be good enough.
getUniqueId :: IO Integer
getUniqueId
 	= randomRIO (0, 1000000000)	

-- Errors -----------------------------------------------------------------------------------------
-- | Throw an error in the build monad.
throw :: BuildError -> Build a
throw	= throwError


-- | Throw a needs error saying we needs the given file.
--   A catcher could then usefully create the file, or defer the compuation until it has been 
--   created.
needs :: FilePath -> Build ()
needs filePath
 = do	isFile	<- io $ doesFileExist filePath
	isDir	<- io $ doesDirectoryExist filePath
	
	if isFile || isDir
	 then return ()
	 else throw $ ErrorNeeds filePath


-- Utils ------------------------------------------------------------------------------------------
-- | Lift an IO action into the build monad.
--   If the action throws any exceptions they get caught and turned into
--   `ErrorIOError` exceptions in our `Build` monad.
io :: IO a -> Build a
io x
 = do	-- catch IOError exceptions
	result	<- liftIO $ try x
	
	case result of
	 Left err	-> throw $ ErrorIOError err
	 Right res	-> return res


-- | Like `when`, but with teh monadz.
whenM :: Monad m => m Bool -> m () -> m ()
whenM cb cx
 = do	b	<- cb
	if b then cx else return ()


-- Output -----------------------------------------------------------------------------------------
-- | Print some text to stdout.
out :: Pretty a => a -> Build ()
out str	
 = io 
 $ do	putStr   $ render $ ppr str
	hFlush stdout

-- | Print some text to stdout followed by a newline.
outLn :: Pretty a => a -> Build ()
outLn str	= io $ putStrLn $ render $ ppr str


-- | Print a blank line to stdout
outBlank :: Build ()
outBlank	= out $ text "\n"


-- | Print a @-----@ line to stdout 
outLine :: Build ()
outLine 	= io $ putStr (replicate 80 '-' ++ "\n")


-- | Print a @=====@ line to stdout
outLINE :: Build ()
outLINE		= io $ putStr (replicate 80 '=' ++ "\n")
	
