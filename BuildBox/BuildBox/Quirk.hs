
module BuildBox.Quirk
	(Quirk	(..))
where
import System.Exit
import BuildBox.Aspect.Units
import BuildBox.Pretty


-- | A Quirk is some extended information about a benchmark or test that isn't represented
--   by an `Aspect`. These are singleton pieces of data where it doesn't make sense to 
--   average them or compute other statistics.
data Quirk
	= QuirkSucceeded
	| QuirkFailed
	| QuirkExitCode	ExitCode
	| QuirkTimeout	Seconds
	deriving (Eq, Ord, Read, Show)
	
instance Pretty Quirk where
 ppr quirk
  = case quirk of
	QuirkSucceeded	
	 -> text "succeeded"

	QuirkFailed
	 -> text "failed"

	QuirkExitCode ExitSuccess
	 -> text "exited successfully"

	QuirkExitCode (ExitFailure code)
	 -> text "exited with failure code " <> int code
	
	QuirkTimeout seconds
	 -> text "timed out after " <> ppr seconds

