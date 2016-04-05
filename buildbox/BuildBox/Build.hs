
-- | Defines the main `Build` monad and common utils.
module BuildBox.Build 
        ( module BuildBox.Build.Testable
        , module BuildBox.Build.BuildState
        , Build

        -- * Building
        , runBuild
        , runBuildWithState
        , runBuildPrint
        , runBuildPrintWithState
        , successfully

        -- * Errors
        , BuildError    (..)
        , throw
        , catch
        , needs

        -- * Utils
        , io
        , whenM

        -- * Output
        , out
        , outLn
        , outBlank
        , outLine
        , outLINE
        , logSystem)

where
import BuildBox.Build.Base
import BuildBox.Build.Testable
import BuildBox.Build.BuildState
import BuildBox.Build.BuildError
import Control.Monad.State
import Control.Monad.Catch
import System.IO
import Prelude


-- | Alias for 'throwM' from Control.Monad.Catch.
throw :: (MonadThrow m, Exception e) => e -> m a
throw = throwM


-- | Log a system command to the handle in our `BuildConfig`, if any.
logSystem :: String -> Build ()
logSystem cmd
 = do   mHandle <- gets buildStateLogSystem
        case mHandle of
         Nothing        -> return ()
         Just h    
          -> do io $ hPutStr   h "buildbox system: "
                io $ hPutStrLn h cmd
                return ()


