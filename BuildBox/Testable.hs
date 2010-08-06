
module BuildBox.Testable
	(Testable(..))
where
import BuildBox.Build	
import Control.Monad.Error

-- | Some state of the system we can test for.
--   Running the test entails an IO action.
--   The test can return true, false, or fail with some error.
--
class Testable prop where
  test :: prop -> Build Bool


-- | Testable properties are checkable.
check :: (Show prop, Testable prop) => prop -> Build ()
check prop
 = do	result	<- test prop
	if result
	 then return ()
	 else throwError $ ErrorTestFailed prop