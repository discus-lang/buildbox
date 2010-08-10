{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ExistentialQuantification #-}

module BuildBox.Build.Base where
import BuildBox.Pretty
import Control.Monad.Error
import System.IO

-- | The builder monad encapsulates and IO action that can fail with an error.
type Build a 	= ErrorT BuildError IO a

-- | The errors we recognise. They're encoded like this so we can produce nice error messages.
data BuildError
	-- | Some generic error
	= ErrorOther String

	-- | Some system command failed.
	| ErrorSystemCmdFailed String

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

	ErrorCheckFailed expected prop
	 -> "check failure: " ++ show prop ++ " expected " ++ show expected


-- | Throw an error in the build monad.
throw :: BuildError -> Build a
throw	= throwError


-- | Run a build command.
runBuild :: Build a -> IO (Either BuildError a)
runBuild build
	= runErrorT build


-- | Run a build command, reporting whether it succeeded to the console.
--   If it succeeded then return Just the result, else Nothing.
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
		

-- | Lift an IO action into the build monad.
io :: IO a -> Build a
io x		= liftIO x


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