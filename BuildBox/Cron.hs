{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | A simple ``cron`` loop. Used for running commands according to a given schedule.
module BuildBox.Cron
	( module BuildBox.Cron.Schedule
	, cronLoop )
where
import BuildBox.Build
import BuildBox.Cron.Schedule
import BuildBox.Command.Sleep
import Data.Time

-- | Given a schedule of commands, run them when they're time is due.
--   This only runs one command at a time. If several commands could be started at a specific
--   moment, then we take the one with the earliest potential start time. If any command throws
--   an error in the `Build` monad then the whole loop does.
--
cronLoop :: Schedule (Build ())-> Build ()
cronLoop schedule
 = do	startTime	<- io $ getCurrentTime

	case earliestEventToStartNow startTime $ eventsOfSchedule schedule of
	 Nothing 
	  -> do	sleep 1
		cronLoop schedule
		
	 Just event 
	  -> do	let Just build	= lookupCommandOfSchedule (eventName event) schedule
		build
		endTime		<- io $ getCurrentTime

		let event'	= event
				{ eventLastStarted	= Just startTime
				, eventLastEnded	= Just endTime }

		let schedule'	= adjustEventOfSchedule event' schedule
		cronLoop schedule'
				
		
		
	