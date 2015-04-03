
-- | What do build 'bots dream about?
module BuildBox.Command.Sleep
        ( sleep
        , msleep)
where
import BuildBox.Build
import Control.Concurrent


-- | Sleep for a given number of seconds.
sleep :: Int -> Build ()
sleep secs      
        = io $ threadDelay $ secs * 1000000

        
-- | Sleep for a given number of milliseconds.
msleep :: Int -> Build ()
msleep msecs
        = io $ threadDelay $ msecs * 1000
