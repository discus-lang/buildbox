
-- | A gang consisting of a fixed number of threads that can run actions in parallel.
--   Good for constructing parallel test frameworks.
module BuildBox.Control.Gang
        ( Gang
        , GangState(..)

        , forkGangActions
        , joinGang
        , pauseGang
        , resumeGang
        , flushGang
        , killGang
        
        , getGangState
        , waitForGangState)
where
import Control.Concurrent
import Control.Exception
import Data.IORef
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- Gang -----------------------------------------------------------------------
-- | Abstract gang of threads.
data Gang
        = Gang 
        { gangThreads           :: Int

        -- | Number of worker threads currently waiting.
        , gangThreadsAvailable  :: QSemN

        , gangState             :: IORef GangState

        , gangActionsRunning    :: IORef Int 

        , gangThreadsRunning    :: IORef (Set ThreadId) }


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
        
        -- | Gang was killed, all the threads are dead (or dying).
        | GangKilled
        deriving (Show, Eq)


-- | Get the state of a gang.
getGangState :: Gang -> IO GangState
getGangState gang
        = readIORef (gangState gang)


-- | Block until all actions have finished executing,
--   or the gang is killed.
joinGang :: Gang -> IO ()
joinGang !gang
 = do   
        -- Wait for all the threads to become available.
        waitQSemN (gangThreadsAvailable gang)
                  (gangThreads gang)

        -- See what state the gang is now in.
        state   <- readIORef (gangState gang)

        if state == GangFinished || state == GangKilled
         -- The gang is done.
         then return ()

         -- Hmm. We're holding all the threads but the gang is still
         -- running. Maybe the controller hasn't started the first
         -- one yet. Just put them all back and try again.
         -- We delay for a moment to allow the controller to run.
         else do
                threadDelay 1000

                signalQSemN (gangThreadsAvailable gang)
                            (gangThreads gang)

                joinGang gang


-- | Block until already started actions have completed, but don't start any more.
--   Gang state changes to `GangFlushing`.
flushGang :: Gang -> IO ()
flushGang !gang
 = do   atomicWriteIORef (gangState gang) GangFlushing
        waitForGangState gang GangFinished


-- | Pause a gang. Actions that have already been started continue to run, 
--   but no more will be started until a `resumeGang` command is issued.
--   Gang state changes to `GangPaused`.
pauseGang :: Gang -> IO ()
pauseGang !gang
 = do   -- Set the gang to paused mode.
        -- This will prevent any new threads from being started.
        atomicWriteIORef (gangState gang) GangPaused


-- | Resume a paused gang, which allows it to continue starting new actions.
--   If the gang was paused it now changes to `GangRunning`,
--   otherwise nothing happens.
resumeGang :: Gang -> IO ()
resumeGang !gang
 = atomicModifyIORef' (gangState gang) $ \state 
 -> do  case state of 
         GangPaused     -> (GangRunning, ())
         _              -> (state, ())


-- | Kill all the threads in a gang.
--   Gang stage changes to `GangKilled`.
killGang :: Gang -> IO ()
killGang !gang
 = do   
        atomicWriteIORef (gangState gang) GangKilled

        tids    <- readIORef (gangThreadsRunning gang) 
        mapM_ killThread $ Set.toList tids

        -- Signal that all the threads are available.
        --
        -- NOTE: There is a race here where a thread might have terminated
        -- cleanly and already bumped the QSemN just before were to add 
        -- to it, but we've killed the gang anyway so nothing more will
        -- be run.
        signalQSemN (gangThreadsAvailable gang) (length tids)


-- | Block until the gang is in the given state.
waitForGangState :: Gang -> GangState -> IO ()
waitForGangState !gang !waitState
 = do   
        -- Wait for all the threads to become available.
        waitQSemN (gangThreadsAvailable gang)
                  (gangThreads gang)

        state   <- readIORef (gangState gang)

        if state == waitState
         then return ()
         else do
                threadDelay 1000

                signalQSemN (gangThreadsAvailable gang)
                            (gangThreads gang)

                waitForGangState gang waitState


-- | Fork a new gang to run the given actions.
--   This function returns immediately, with the gang executing in the background.
--   Gang state starts as `GangRunning` then transitions to `GangFinished`.
--   To block until all the actions are finished use `joinGang`.
forkGangActions
        :: Int          -- ^ Number of worker threads in the gang \/ maximum number
                        --   of actions to execute concurrenty.
        -> [IO ()]      -- ^ Actions to run. They are started in-order, but may finish
                        --   out-of-order depending on the run time of the individual action.
        -> IO Gang

forkGangActions !threads !actions
 = do   semThreads              <- newQSemN threads
        refState                <- newIORef GangRunning
        refActionsRunning       <- newIORef 0
        refThreadsRunning       <- newIORef (Set.empty)
        let gang        
                = Gang
                { gangThreads           = threads
                , gangThreadsAvailable  = semThreads 
                , gangState             = refState
                , gangActionsRunning    = refActionsRunning 
                , gangThreadsRunning    = refThreadsRunning }

        _ <- forkIO $ gangLoop gang actions
        return gang
        

-- | Run actions on a gang.
gangLoop :: Gang -> [IO ()] -> IO ()

gangLoop !gang []
 = do   -- Signal that the gang is finished running actions.
        writeIORef (gangState gang) GangFinished

gangLoop !gang actions@(action:actionsRest)
 = do   
        -- Wait for a worker thread to become available.
        waitQSemN  (gangThreadsAvailable gang) 1
        
        -- See what state the gang is in.
        state   <- readIORef (gangState gang)

        case state of
         GangRunning 
          -> do -- Fork off the next action.
                -- We need to use the 'finally' to release the thread
                -- in the case that the action throws an exception.
                tid     <- forkOS 
                        $  finally action
                        $  do   -- Remove our ThreadId from the set of
                                -- running ThreadIds.
                                tid     <- myThreadId
                                atomicModifyIORef' (gangThreadsRunning gang)
                                        (\tids -> (Set.delete tid tids, ()))

                                -- Signal that the worker is now available.
                                signalQSemN (gangThreadsAvailable gang) 1
        
                -- Add the ThreadId of the freshly forked thread to the set
                -- of running ThreadIds. We'll need this set if we want to kill
                -- the gang.
                atomicModifyIORef' (gangThreadsRunning gang)
                        (\tids -> (Set.insert tid tids, ()))
        
                -- Handle the rest of the actions.
                gangLoop gang actionsRest

         -- TODO: avoid spinning on pause.
         GangPaused
          -> do signalQSemN (gangThreadsAvailable gang) 1
                threadDelay 1000
                gangLoop gang actions
                        
         GangFlushing   -> return ()
         GangFinished   -> return ()
         GangKilled     -> return ()


