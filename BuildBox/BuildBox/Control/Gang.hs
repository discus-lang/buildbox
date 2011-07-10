
-- | A gang consisting of a fixed number of threads that can run actions in parallel.
--   Good for constructing parallel test frameworks.
module BuildBox.Control.Gang
	( Gang
	, GangState(..)

	, forkGangActions
	, joinGang
	, flushGang
	, pauseGang
	, resumeGang
	
	, getGangState
	, waitForGangState)
where
import Control.Concurrent
import Data.IORef

-- Gang -----------------------------------------------------------------------
-- | Abstract gang of threads.
data Gang
	= Gang 
	{ gangThreads		:: Int
	, gangThreadsAvailable	:: QSemN
	, gangState		:: IORef GangState
	, gangActionsRunning	:: IORef Int }

data GangState
	= -- | Gang is running and starting new actions.
	  GangRunning

	-- | Gang may be running already started actions, 
	--   but no new ones are being started.
	| GangPaused

	-- | Gang is waiting for currently running actions to finish, 
	--   but not starting new ones.
	| GangFlushing

	-- | Gang is finished, all the actions have completed.
	| GangFinished
	deriving (Show, Eq)


-- | Get the state of a gang.
getGangState :: Gang -> IO GangState
getGangState gang
	= readIORef (gangState gang)


-- | Block until all actions have finished executing.
joinGang :: Gang -> IO ()
joinGang gang
 	= waitForGangState gang GangFinished 


-- | Block until already started actions have completed, but don't start any more.
--   Gang state changes to `GangFlushing`.
flushGang :: Gang -> IO ()
flushGang gang
 = do	writeIORef (gangState gang) GangFlushing
	waitForGangState gang GangFinished


-- | Pause a gang. Actions that have already been started continue to run, 
--   but no more will be started until a `resumeGang` command is issued.
--   Gang state changes to `GangPaused`.
pauseGang :: Gang -> IO ()
pauseGang gang
 	= writeIORef (gangState gang) GangPaused


-- | Resume a paused gang, which allows it to continue starting new actions.
--   Gang state changes to `GangRunning`
resumeGang :: Gang -> IO ()
resumeGang gang
	= writeIORef (gangState gang) GangRunning


-- | Block until the gang is in the given state.
waitForGangState :: Gang -> GangState -> IO ()
waitForGangState gang waitState
 = do	state	<- readIORef (gangState gang)
	if state == waitState
	 then return ()
	 else do
		threadDelay 10000
		waitForGangState gang waitState


-- | Fork a new gang to run the given actions.
--   This function returns immediately, with the gang executing in the background.
--   Gang state starts as `GangRunning` then transitions to `GangFinished`.
--   To block until all the actions are finished use `joinGang`.
forkGangActions
	:: Int 			-- ^ Number of worker threads in the gang \/ maximum number
				--   of actions to execute concurrenty.
	-> [IO ()] 		-- ^ Actions to run. They are started in-order, but may finish
				--   out-of-order depending on the run time of the individual action.
	-> IO Gang

forkGangActions threads actions
 = do	semThreads		<- newQSemN threads
	refState		<- newIORef GangRunning
	refActionsRunning	<- newIORef 0
	let gang	
		= Gang
		{ gangThreads		= threads
		, gangThreadsAvailable	= semThreads 
		, gangState 		= refState
		, gangActionsRunning	= refActionsRunning }

	_ <- forkIO $ gangLoop gang actions
	return gang
	

-- | Run actions on a gang.
gangLoop :: Gang -> [IO ()] -> IO ()
gangLoop gang []
 = do	-- Wait for all the threads to finish.
	waitQSemN 
		(gangThreadsAvailable gang) 
		(gangThreads gang)
		
	-- Signal that the gang is finished running actions.
	writeIORef (gangState gang) GangFinished


gangLoop gang actions@(action:actionsRest)
 = do	state	<- readIORef (gangState gang)
	case state of
	 GangRunning 
	  -> do	-- Wait for a worker thread to become available.
		waitQSemN (gangThreadsAvailable gang) 1
		gangLoop_withWorker gang action actionsRest

	 GangPaused
	  -> do	threadDelay 100000
	 	gangLoop gang actions
			
	 GangFlushing
	  -> do	actionsRunning	<- readIORef (gangActionsRunning gang)
		if actionsRunning == 0
		 then	writeIORef (gangState gang) GangFinished
		 else do	
			threadDelay 100000
			gangLoop gang []

	 GangFinished
	  -> return ()


-- we have an available worker
gangLoop_withWorker :: Gang -> IO () -> [IO ()] -> IO ()
gangLoop_withWorker gang action actionsRest
 = do	-- See if we're supposed to be starting actions or not.
	state	<- readIORef (gangState gang)
	case state of
	 GangRunning
	  -> do	-- fork off the first action
		_ <- forkOS $ do
			-- run the action (and wait for it to complete)
			action

			-- signal that a new worker is available
			signalQSemN (gangThreadsAvailable gang) 1
	
		-- handle the rest
		gangLoop gang actionsRest

	 -- someone issued flush or pause command while we
	 -- were waiting for a worker, so don't start next action.
	 _ -> gangLoop gang (action:actionsRest)
