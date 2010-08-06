{-# LANGUAGE ExistentialQuantification #-}

-- | Builder monad.
--   Encapsulates an IO action that can fail with some error.
--   The errors are usually things we've checked for explicitly, such as the existence of files.
--
module BuildBox.Build.Base
	( Build
	, BuildError(..)
	, throw
	, runBuild
	, runBuildAndPrintResult
	, io
	, out
	, outLn
	, outBlank
	, outLine
	, outLINE
	, whenM)
	
where
import Control.Monad.Error
import System.IO

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


-- | Throw an error in the build monad.
throw :: BuildError -> Build a
throw	= throwError


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
	  -> do	putStrLn "\nBuild failed"
		putStr   "  due to "
		putStrLn $ show err
		return $ Nothing
		
	 Right x
	  -> do	putStrLn "Build succeeded."
		return $ Just x
		

-- | Lift an IO command into the build monad.
io x		= liftIO x


-- | Print some text to stdout.
out str	
 = io 
 $ do	putStr   str
	hFlush stdout

-- | Print some text to stdout followed by a newline.
outLn :: String -> Build ()
outLn str	= io $ putStrLn str


-- | Print a blank line to stdout
outBlank :: Build ()
outBlank	= out "\n"


-- | Print a ----- line to stdout 
outLine :: Build ()
outLine 	= io $ putStr (replicate 80 '-' ++ "\n")


-- | Print a ===== line to stdout
outLINE :: Build ()
outLINE		= io $ putStr (replicate 80 '=' ++ "\n")


whenM :: Monad m => m Bool -> m () -> m ()
whenM cb cx
 = do	b	<- cb
	if b then cx else return ()