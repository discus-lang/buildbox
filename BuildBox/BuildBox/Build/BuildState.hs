{-# OPTIONS_HADDOCK hide #-}

module BuildBox.Build.BuildState
	( BuildState(..)
	, buildStateDefault)
where
import System.IO

-- BuildConfig ------------------------------------------------------------------------------------
-- | Global builder configuration.
data BuildState
	= BuildState
	{ -- | Log all system commands executed to this file handle.
	  buildStateLogSystem	:: Maybe Handle
	
	  -- | Uniqueish id for this build process.
	  --   On POSIX we'd use the PID, but that doesn't work on Windows.
	  --   The id is initialised by the Haskell random number generator on startup.
	, buildStateId		:: Integer
	
	  -- | Sequence number for generating fresh file names.
	, buildStateSeq		:: Integer 
	
	  -- | Scratch directory for making temp files.
	, buildStateScratchDir	:: FilePath }


-- | The default build config.
buildStateDefault :: Integer -> FilePath -> BuildState
buildStateDefault uniqId scratchDir 
	= BuildState
	{ buildStateLogSystem	= Nothing
	, buildStateId		= uniqId
	, buildStateSeq		= 0 
	, buildStateScratchDir	= scratchDir }

