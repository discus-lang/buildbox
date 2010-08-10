
module Config 
	(Config(..))
where

-- | Buildbot command line configuration.
data Config
	= Config
	{ configVerbose		:: Bool
	, configTmpDir		:: String
	, configDoBuild		:: Bool
	, configDoTest		:: Bool 
	, configIterations	:: Int
	, configWriteResults	:: Maybe FilePath
	, configWithResults	:: Maybe FilePath }
	deriving Show
