
-- | Sending email. 
--   If you're on a system with a working @sendmail@ then use that.
--   Otherwise, the stand-alone @msmtp@ server is easy to set up.
--   Get @msmtp@ here: <http://msmtp.sourceforge.net>
module BuildBox.Command.Mail
	( Mail(..)
	, Mailer(..)
	, createMailWithCurrentTime
	, renderMail
	, sendMailWithMailer)
where
import BuildBox.Build
import BuildBox.Pretty
import BuildBox.Command.Environment
import BuildBox.Command.System
import System.Locale	(defaultTimeLocale)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Calendar


-- | An email message that we can send.
data Mail
	= Mail
	{ mailFrom		:: String
	, mailTo		:: String
	, mailSubject		:: String
	, mailTime		:: UTCTime
	, mailTimeZone		:: TimeZone
	, mailMessageId		:: String
	, mailBody		:: String }
	deriving Show


-- | An external mailer that can send messages.
--   	Also contains mail server info if needed.
data Mailer
	-- | Send the mail by writing to the stdin of this command.
	--   On many systems the command 'sendmail' will be aliased to an appropriate
	--   wrapper for whatever Mail Transfer Agent (MTA) you have installed.
	= MailerSendmail
	{ mailerPath		:: FilePath
	, mailerExtraFlags	:: [String] }

	-- | Send mail via MSMTP, which is a stand-alone SMTP sender.
	--   This might be be easier to set up if you don't have a real MTA installed.
	--   Get it from http://msmtp.sourceforge.net/
	| MailerMSMTP
	{ mailerPath		:: FilePath
	, mailerPort		:: Maybe Int }
	deriving Show


-- | Create a mail with a given from, to, subject and body.
--   Fill in the date and message id based on the current time.
--   Valid dates and message ids are needed to prevent the mail
--   being bounced by anti-spam systems.
createMailWithCurrentTime 
	:: String 	-- ^ ''from'' field. Should be an email address.
	-> String	-- ^ ''to'' field. Should be an email address.
	-> String	-- ^ Subject line.
	-> String	-- ^ Message  body.
	-> Build Mail

createMailWithCurrentTime from to subject body
 = do
	-- We need to add the date otherwise our messages will get marked as spam.
	-- Use RFC 2822 format timestamp.
	utime		<- io $ getCurrentTime
	zone		<- io $ getCurrentTimeZone

	-- Generate a messageid based on the clock time.
	hostName	<- getHostName
	let dayNum	= toModifiedJulianDay $ utctDay utime
	let secTime	= utctDayTime utime
	let messageId	=  "<" ++ show dayNum ++ "." ++ (init $ show secTime)
			++ "@" ++ hostName ++ ">"
		
	return	$ Mail
		{ mailFrom	= from
		, mailTo	= to
		, mailSubject	= subject
		, mailTime	= utime
		, mailTimeZone	= zone
		, mailMessageId	= messageId
		, mailBody	= body }


-- | Render an email message as a string.
renderMail :: Mail -> Doc
renderMail mail
 = vcat
	[ ppr "From: "		<> ppr (mailFrom mail)
	, ppr "To: "		<> ppr (mailTo   mail)
	, ppr "Subject: "	<> ppr (mailSubject mail)
	, ppr "Date: "		<> (ppr $ formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z"
					$ utcToZonedTime (mailTimeZone mail) (mailTime mail))

	, ppr "Message-Id: " 	<> ppr (mailMessageId mail)
	, ppr ""
	, ppr (mailBody mail) ]


-- | Send a mail message.
sendMailWithMailer :: Mail -> Mailer -> Build ()
sendMailWithMailer mail mailer
 = case mailer of
	MailerSendmail{}	
	 -> ssystemTee False
		(mailerPath mailer
			++ " -t ") -- read recipients from the mail
		(render $ renderMail mail)

	MailerMSMTP{}	
	 -> ssystemTee False
		(mailerPath mailer 
			++ " -t " -- read recipients from the mail
			++ (maybe "" (\port -> " --port=" ++ show port) $ mailerPort mailer))
		(render $ renderMail mail)
		



