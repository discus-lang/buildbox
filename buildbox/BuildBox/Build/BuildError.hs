{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_HADDOCK hide #-}

module BuildBox.Build.BuildError
        (BuildError(..))
where
import BuildBox.Pretty
import Control.Monad.Catch
import Data.Typeable
import System.Exit
import BuildBox.Data.Log                (Log)
import qualified BuildBox.Data.Log      as Log
import qualified Data.Text              as T


-- BuildError -------------------------------------------------------------------------------------
-- | The errors we recognise.
data BuildError
        -- | Some generic error
        = ErrorOther String

        -- | Some system command fell over, and it barfed out the given stdout and stderr.
        | ErrorSystemCmdFailed
                { buildErrorCmd         :: String
                , buildErrorCode        :: ExitCode
                , buildErrorStdout      :: Log
                , buildErrorStderr      :: Log }

        -- | Some miscellanous IO action failed.
        | ErrorIOError IOError

        -- | Some property `check` was supposed to return the given boolean value, but it didn't.
        | forall prop. Show prop => ErrorCheckFailed Bool prop

        -- | A build command needs the following file to continue.
        --   This can be used for writing make-like bots.
        | ErrorNeeds FilePath
        deriving Typeable

instance Exception BuildError



instance Pretty BuildError where
 ppr err
  = case err of
        ErrorOther str
         -> string "Other error: "
                % string str

        ErrorSystemCmdFailed{}
         -> vcat
         $      [ string "System command failure."
                , string "    command: " % (string $ buildErrorCmd err)
                , string "  exit code: " % (string $ show $ buildErrorCode err)
                , string "" ]

         ++ (if (not $ Log.null $ buildErrorStdout err)
             then [ string "-- stdout (last 10 lines) ------------------------------------------------------"
                  , string $ Log.toString $ Log.lastLines 10 $ buildErrorStdout err]
             else [])

         ++ (if (not $ Log.null $ buildErrorStderr err)
             then [ string "-- stderr (last 10 lines) ------------------------------------------------------"
                  , string $ Log.toString $ Log.lastLines 10 $ buildErrorStderr err ]
             else [])

         ++ (if (  (not $ Log.null $ buildErrorStdout err)
               || (not $ Log.null $ buildErrorStderr err))
             then [ string "--------------------------------------------------------------------------------" ]
             else [])

        ErrorIOError ioerr
         -> string "IO error: "
                % (string $ show ioerr)

        ErrorCheckFailed expected prop
         -> string "Check failure: "
                % (string $ show prop)
                % (string " expected ")
                % (string $ show expected)

        ErrorNeeds filePath
         -> string "Build needs: "
                % string filePath


instance Show BuildError where
 show err = T.unpack $ ppr err


