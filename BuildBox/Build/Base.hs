{-# LANGUAGE ExistentialQuantification #-}

-- | Builder monad.
--   Encapsulates an IO action that can fail with some error.
--   The errors are usually things we've checked for explicitly, such as the existence of files.
--
module BuildBox.Build.Base
	( Build
	, BuildError(..)
	, runBuild
	, io
	, out
	, outLn
	, throw
	, runBuildAndPrintResult)
	
where
import Control.Monad.Error

-- | A build command is an IO action that can fail with an error.
type Build a 	= ErrorT BuildError IO a

-- | The errors we recognise
data BuildError
	-- | Some generic error
	= ErrorOther String

	-- | Some system command failed.
	| ErrorSystemCmdFailed String

	-- | Some testable thing failed in a way we weren't counting on.
	--   This is a "frame work" failure as opposed to an object failure.
	| forall prop. Show prop => ErrorTestFailed prop	

instance Error BuildError where
 strMsg s = ErrorOther s

instance Show BuildError where
 show err
  = case err of
	ErrorOther str
	 -> "other error: " ++ str

	ErrorSystemCmdFailed str
	 -> "system command failure: \"" ++ str ++ "\""

	ErrorTestFailed prop
	 -> "framework test failure: " ++ show prop


throw :: BuildError -> Build a
throw	= throwError

-- | Helpers
io x		= liftIO x
out   str	= io $ putStr   str
outLn str	= io $ putStrLn str


-- | Run a build command.
runBuild :: Build a -> IO (Either BuildError a)
runBuild build
	= runErrorT build

-- | Execute a build command, reporting whether it succeeded or not.
runBuildAndPrintResult :: Build a -> IO (Maybe a)
runBuildAndPrintResult build
 = do	result	<- runErrorT build
	case result of
	 Left err
	  -> do	putStrLn "\n"
		putStrLn "Build failed"
		putStr   "  due to "
		putStrLn $ show err
		return $ Nothing
		
	 Right x
	  -> do	putStrLn "\n"
		putStrLn "Build succeeded."
		return $ Just x
		


