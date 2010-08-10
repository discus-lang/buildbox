
-- | Some property of the system we can test for.
module BuildBox.Build.Testable
	( Testable(..)
	, check
	, checkNot
	, outCheckOk)
where
import System.IO
import BuildBox.Build.Base	
import Control.Monad.Error


-- | Some testable property.
class Testable prop where
  test :: prop -> Build Bool

-- | Testable properties are checkable.
--   If the check returns false then throw an error.
check :: (Show prop, Testable prop) => prop -> Build ()
check prop
 = do	result	<- test prop
	if result
	 then return ()
	 else throwError $ ErrorCheckFailed True prop


-- | Testable properties are checkable.
--   If the check returns true then throw an error.
checkNot :: (Show prop, Testable prop) => prop -> Build ()
checkNot prop
 = do	result	<- test prop
	if result
	 then throwError $ ErrorCheckFailed False prop
	 else return ()
	

-- | Check some property while printing what we're doing.
outCheckOk 
	:: (Show prop, Testable prop) 
	=> String -> prop -> Build ()

outCheckOk str prop
 = do	out $ str ++ "..."
	io  $ hFlush stdout
	check prop
	out " ok\n"

