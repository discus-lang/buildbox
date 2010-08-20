

module BuildBox.Command.Mail
	(Mail(..))
where
import BuildBox.Build
import BuildBox.Pretty
import BuildBox.Command.Environment
import BuildBox.Command.System
import BuildBox.Command.File
import System.Time
import System.Locale	(defaultTimeLocale)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Maybe


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
--	We only support msmtp at the moment.
data Mailer
	= MailerMSMTP
	{ mailerPath		:: FilePath
	, mailerPort		:: Maybe Int }
	deriving Show


-- | Create a mail with a given from, to, subject and body.
--   Fill in the date and message id based on the current time.
createMailWithCurrentTime :: String -> String -> String -> String -> Build Mail
createMailWithCurrentTime from to subject body
 = do
	-- We need to add the date otherwise our messages will get marked as spam.
	-- Use RFC 2822 format timestamp.
	utime		<- io $ getCurrentTime
	zone		<- io $ getCurrentTimeZone
	let dateString	= formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z"
			$ utcToZonedTime zone utime

	-- Generate a messageid based on the clock time.
	date		<- io $ getClockTime 
	hostName	<- getHostName
	let TOD secs psecs	= date
	let messageId	=  "<" ++ show secs ++ "." ++ take 6 (show psecs)
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
	MailerMSMTP{}	-> sendMailWithMSMTP mail mailer
	
sendMailWithMSMTP mail mailer@MailerMSMTP{}
 = withTempFile $ \fileName 
 -> do	let mailDoc	= renderMail mail
	io $ writeFile fileName (render mailDoc)

	systemNull 
		$ "cat " ++ fileName 
		++ " | " ++ mailerPath mailer
		++ " -t "			-- read recipients from the mail
		++ (maybe "" (\port -> " --port=" ++ show port) $ mailerPort mailer)
		
