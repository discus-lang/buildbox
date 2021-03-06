{-# OPTIONS_HADDOCK hide #-}

module BuildBox.Build.Base where
import BuildBox.Pretty
import BuildBox.Build.BuildError
import BuildBox.Build.BuildState
import Control.Monad.Catch
import Control.Monad.State
import System.IO
import System.Directory
import qualified Data.Text      as T


-- | The builder monad encapsulates and IO action that can fail with an error,
--   and also read some global configuration info.
type Build a    = StateT BuildState IO a


-- Build ------------------------------------------------------------------------------------------
-- | Run a build command. The first argument is a directory that can be used for
--   temporary files (like \"/tmp\")
runBuild :: FilePath -> Build a -> IO (Either BuildError a)
runBuild scratchDir build
 = do   let s   = buildStateDefault scratchDir
        try $ evalStateT build s


-- | Like 'runBuild`, but report whether it succeeded to the console.
--   If it succeeded then return Just the result, else Nothing.
runBuildPrint :: FilePath -> Build a -> IO (Maybe a)
runBuildPrint scratchDir build
 = do   let s   = buildStateDefault scratchDir
        runBuildPrintWithState s build


-- | Like `runBuild` but also takes a `BuildState`.
runBuildWithState :: BuildState -> Build a -> IO (Maybe a)
runBuildWithState s build
 = do   result  <- try $ evalStateT build s
        case result of
         Left (err :: BuildError)
          -> do putStrLn $ T.unpack $ ppr err
                return $ Nothing

         Right x
          -> do return $ Just x


-- | Like `runBuildPrint` but also takes a `BuildState`.
runBuildPrintWithState :: BuildState -> Build a -> IO (Maybe a)
runBuildPrintWithState s build
 = do   result  <- try $ evalStateT build s
        case result of
         Left (err :: BuildError)
          -> do putStrLn "\nBuild failed"
                putStr   "  due to "
                putStrLn $ T.unpack $ ppr err
                return $ Nothing

         Right x
          -> do putStrLn "Build succeeded."
                return $ Just x


-- | Discard the resulting value of a compuation.
--   Used like @successfully . runBuild ...@
successfully :: IO a -> IO ()
successfully f  = f >> return ()


-- Errors -----------------------------------------------------------------------------------------
-- | Throw a needs error saying we needs the given file.
--   A catcher could then usefully create the file, or defer the compuation until it has been
--   created.
needs :: FilePath -> Build ()
needs filePath
 = do   isFile  <- io $ doesFileExist filePath
        isDir   <- io $ doesDirectoryExist filePath

        if isFile || isDir
         then return ()
         else throwM $ ErrorNeeds filePath


-- Utils ------------------------------------------------------------------------------------------
-- | Lift an IO action into the build monad.
--   If the action throws any exceptions they get caught and turned into
--   `ErrorIOError` exceptions in our `Build` monad.
io :: IO a -> Build a
io x
 = do   -- catch IOError exceptions
        result  <- liftIO $ try x

        case result of
         Left  err      -> throwM $ ErrorIOError err
         Right res      -> return res


-- | Like `when`, but with teh monadz.
whenM :: Monad m => m Bool -> m () -> m ()
whenM cb cx
 = do   b       <- cb
        if b then cx else return ()


-- Output -----------------------------------------------------------------------------------------
-- | Print some text to stdout.
out :: Text -> Build ()
out tx
 = io
 $ do   putStr $ T.unpack tx
        hFlush stdout

-- | Print some text to stdout followed by a newline.
outLn :: Text -> Build ()
outLn tx        = io $ putStrLn $ T.unpack tx


-- | Print a blank line to stdout
outBlank :: Build ()
outBlank        = out $ string "\n"


-- | Print a @-----@ line to stdout
outLine :: Build ()
outLine         = io $ putStr (replicate 80 '-' ++ "\n")


-- | Print a @=====@ line to stdout
outLINE :: Build ()
outLINE         = io $ putStr (replicate 80 '=' ++ "\n")

