
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
	, configWithGhcBuild	:: Maybe FilePath
	, configWithGhc		:: FilePath
	, configWithGhcPkg	:: FilePath
	, configDoUnpack	:: Bool
	, configDoBuild		:: Bool
	, configDoTest		:: Bool 
	, configIterations	:: Int
	, configWriteResults	:: Maybe FilePath
	, configAgainstResults	:: Maybe FilePath 
	, configMailFromTo	:: Maybe (String, String) }
	deriving Show


-- Hard coded config.
defaultMailer
	= MailerMSMTP
	{ mailerPath	= "msmtp"
	, mailerPort	= Just 587 }