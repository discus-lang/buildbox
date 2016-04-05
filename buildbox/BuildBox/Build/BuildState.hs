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
          buildStateLogSystem   :: Maybe Handle
                
          -- | Sequence number for generating fresh file names.
        , buildStateSeq         :: Integer 
        
          -- | Scratch directory for making temp files.
        , buildStateScratchDir  :: FilePath }


-- | The default build config.
buildStateDefault :: FilePath -> BuildState
buildStateDefault scratchDir 
        = BuildState
        { buildStateLogSystem   = Nothing
        , buildStateSeq         = 0 
        , buildStateScratchDir  = scratchDir }

