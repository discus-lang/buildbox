{-# LANGUAGE ExistentialQuantification #-}

module BuildBox.Build
	( Build
	, BuildError(..)
	, io)
where
import Control.Monad.Error
	
-- | A build command is an IO action that can fail with an error.
type Build a 	= ErrorT BuildError IO a

-- | The errors we recognise
data BuildError
	-- | Some generic error
	= ErrorOther String

	-- | Some system command failed.
	| ErrorCmdFailed String

	-- | Some testable thing failed in a way we weren't counting on.
	--   This is an "infrastructure" failure as opposed to an object failure.
	| forall prop. Show prop => ErrorTestFailed prop	

instance Error BuildError where
 strMsg s = ErrorOther s


-- | Helpers
io x		= liftIO x

