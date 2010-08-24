
-- | Time utils useful for writing buildbots.
module BuildBox.Time
	( module Data.Time
	, readLocalTimeOfDayAsUTC
	, getStampyTime
	, getMidnightTodayLocal
	, getMidnightTodayUTC
	, getMidnightTomorrowLocal
	, getMidnightTomorrowUTC)
where
import Data.Time
import System.Locale


-- | Read a time of day string like @17:34:05@ in the local time zone
--   and convert that to a UTC time of day. Good for parsing command line args to buildbots.
readLocalTimeOfDayAsUTC :: String -> IO TimeOfDay
readLocalTimeOfDayAsUTC str
 = do	
	-- Get the current timeZone.
	curTime	<- getZonedTime

	-- Convert the time of day to what it would be as UTC.
	let (_, timeOfDayUTC)
		=  localToUTCTimeOfDay 
			(zonedTimeZone curTime)
			(read str) 

	return timeOfDayUTC


-- | Get a local time stamp with format YYYYMMDD_HHMMSS. Good for naming files with.
getStampyTime :: IO String
getStampyTime
 = do	time	<- getZonedTime
	return	$  formatTime defaultTimeLocale "%Y%m%d_%k%M%S" time


-- | Get the local midnight we've just had as a `LocalTime`.
getMidnightTodayLocal :: IO LocalTime
getMidnightTodayLocal
 = do	curTime	<- getZonedTime
	return	$ LocalTime
		{ localDay		= localDay $ zonedTimeToLocalTime curTime
		, localTimeOfDay	= midnight }


-- | Get the local midnight that we've just had, as a `UTCTime`.
getMidnightTodayUTC :: IO UTCTime 
getMidnightTodayUTC
 = do	curTime	<- getZonedTime
	return	$ zonedTimeToUTC
		$ ZonedTime
			(LocalTime	
				{ localDay		= localDay $ zonedTimeToLocalTime curTime
				, localTimeOfDay	= midnight })
			(zonedTimeZone curTime)


-- | Get the local midnight we're about to have as a `LocalTime`.
getMidnightTomorrowLocal :: IO LocalTime
getMidnightTomorrowLocal
 = do	curTime	<- getZonedTime
	return	$ LocalTime
		{ localDay		= addDays 1 (localDay (zonedTimeToLocalTime curTime)) 
		, localTimeOfDay	= midnight }

-- | Get the local midnight we're about to have as a `UTCTime`.
getMidnightTomorrowUTC 	 :: IO UTCTime
getMidnightTomorrowUTC
 = do	curTime	<- getZonedTime
	return	$ zonedTimeToUTC
		$ ZonedTime
		  	(LocalTime
				{ localDay		= addDays 1 (localDay (zonedTimeToLocalTime curTime)) 
				, localTimeOfDay	= midnight })
			(zonedTimeZone curTime)


	
	