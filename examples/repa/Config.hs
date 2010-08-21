
module Config 
	( Config(..)
	, defaultMailer)
where
import BuildBox

-- | Buildbot command line configuration.
data Config
	= Config
	{ configVerbose		:: Bool
	, configTmpDir		:: String

	-- GHC config
	, configWithGhcBuild	:: Maybe FilePath
	, configWithGhc		:: FilePath
	, configWithGhcPkg	:: FilePath

	, configWithGhcSnapshot	:: Maybe FilePath

	-- Build stages
	, configDoUnpackGhc	:: Bool
	, configDoBuildGhc	:: Bool
	, configDoUnpack	:: Bool
	, configDoBuild		:: Bool
	, configDoTest		:: Bool 

	-- Testing config.
	, configIterations	:: Int
	, configAgainstResults	:: Maybe FilePath 

	-- What do with the results.
	, configWriteResults	:: Maybe FilePath
	, configMailFromTo	:: Maybe (String, String) }
	deriving Show


-- Hard coded config.
defaultMailer
	= MailerMSMTP
	{ mailerPath	= "msmtp"
	, mailerPort	= Just 587 }