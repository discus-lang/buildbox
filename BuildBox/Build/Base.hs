{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ExistentialQuantification #-}

module BuildBox.Build.Base where
import BuildBox.Pretty
import Control.Monad.Error
import Control.Monad.Reader
import System.IO
import System.IO.Error

-- BuildError -------------------------------------------------------------------------------------
-- | The errors we recognise.
data BuildError
	-- | Some generic error
	= ErrorOther String

	-- | Some system command failed.
	| ErrorSystemCmdFailed String

	-- | Some other IO action failed.
	| ErrorIOError IOError

	-- | Some property `check` was supposed to return the given boolean value, but it didn't.
	| forall prop. Show prop => ErrorCheckFailed Bool prop	

instance Error BuildError where
 strMsg s = ErrorOther s

instance Show BuildError where
 show err
  = case err of
	ErrorOther str
	 -> "other error: " ++ str

	ErrorSystemCmdFailed str
	 -> "system command failure: \"" ++ str ++ "\""

	ErrorIOError ioerr
	 -> "IO error: " ++ show ioerr

	ErrorCheckFailed expected prop
	 -> "check failure: " ++ show prop ++ " expected " ++ show expected

-- | Throw an error in the build monad.
throw :: BuildError -> Build a
throw	= throwError


-- BuildConfig ------------------------------------------------------------------------------------
-- | Global builder configuration.
data BuildConfig
	= BuildConfig
	{ -- | Log all system commands executed to this file handle.
	  buildConfigLogSystem	:: Maybe Handle }

buildConfigInit
	= BuildConfig
	{ buildConfigLogSystem	= Nothing }

-- | Log a system command.
logSystem :: String -> Build ()
logSystem cmd
 = do	mHandle	<- asks buildConfigLogSystem
	case mHandle of
	 Nothing	-> return ()
	 Just handle	
	  -> do	io $ hPutStr handle cmd
		return ()

-- Build ------------------------------------------------------------------------------------------
-- | The builder monad encapsulates and IO action that can fail with an error, 
--   and also read some global configuration info.
type Build a 	= ErrorT BuildError (ReaderT BuildConfig IO) a


-- | Run a build command.
runBuild :: Build a -> IO (Either BuildError a)
runBuild build
	= runReaderT (runErrorT build) buildConfigInit


-- | Run a build command, reporting whether it succeeded to the console.
--   If it succeeded then return Just the result, else Nothing.
runBuildAndPrintResult :: Build a -> IO (Maybe a)
runBuildAndPrintResult build
 = do	result	<- runReaderT (runErrorT build) buildConfigInit
	case result of
	 Left err
	  -> do	putStrLn "\nBuild failed"
		putStr   "  due to "
		putStrLn $ show err
		return $ Nothing
		
	 Right x
	  -> do	putStrLn "Build succeeded."
		return $ Just x
		

-- Utils ------------------------------------------------------------------------------------------
-- | Lift an IO action into the build monad.
io :: IO a -> Build a
io x
 = do	-- catch IOError exceptions
	result	<- liftIO $ try x
	
	case result of
	 Left err	-> throw $ ErrorIOError err
	 Right res	-> return res


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


-- | Like `when`, but with teh monadz.
whenM :: Monad m => m Bool -> m () -> m ()
whenM cb cx
 = do	b	<- cb
	if b then cx else return ()