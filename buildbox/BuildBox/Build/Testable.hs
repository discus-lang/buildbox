
-- | Some property of the system we can test for.
--
--   They have `Show` instances so we can make nice error messages if a `check` fails.
module BuildBox.Build.Testable
        ( Testable(..)
        , check
        , checkFalse
        , outCheckOk
        , outCheckFalseOk)
where
import BuildBox.Build.Base      
import BuildBox.Build.BuildError
import Control.Monad.Catch


-- | Some testable property.
class Testable prop where
  test :: prop -> Build Bool


-- | Testable properties are checkable.
--   If the check returns false then throw an error.
check :: (Show prop, Testable prop) => prop -> Build ()
check prop
 = do   result  <- test prop
        if result
         then return ()
         else throwM $ ErrorCheckFailed True prop


-- | Testable properties are checkable.
--   If the check returns true then throw an error.
checkFalse :: (Show prop, Testable prop) => prop -> Build ()
checkFalse prop
 = do   result  <- test prop
        if result
         then throwM $ ErrorCheckFailed False prop
         else return ()
        

-- | Check some property while printing what we're doing.
outCheckOk 
        :: (Show prop, Testable prop) 
        => String -> prop -> Build ()

outCheckOk str prop
 = do   outLn str
        check prop


-- | Check some property while printing what we're doing.
outCheckFalseOk 
        :: (Show prop, Testable prop) 
        => String -> prop -> Build ()

outCheckFalseOk str prop
 = do   outLn str
        checkFalse prop

