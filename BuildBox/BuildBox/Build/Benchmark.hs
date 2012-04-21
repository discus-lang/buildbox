
module BuildBox.Build.Benchmark
        ( Benchmark     (..)
        , BenchResult   (..)
        , runBenchmark
        , iterateBenchmark
        , timeBuild)
where
import BuildBox.Build.Base
import BuildBox.Data.Physical
import Data.Time

-- | Benchmark definition.
data Benchmark result
        = forall a. Benchmark
        { -- | A unique name for the benchmark
          benchmarkName         :: String

          -- | Setup command to run before the main benchmark.
          --   This does not contribute to the reported time of the overall result.
        , benchmarkSetup        :: Build ()

          -- | The main command to benchmark.
        , benchmarkCommand      :: Build a

          -- | Check and post-process the result of the main command.
          --   This does not contribute to the reported time of the overall result.
        , benchmarkCheck        :: a -> Build result }


-- | Benchmark result.
data BenchResult result
        = BenchResult
        { benchResultName       :: String
        , benchResultIteration  :: Int
        , benchResultTime       :: Seconds
        , benchResultValue      :: result }


-- | Run a benchmark a single time.
runBenchmark :: Benchmark result -> Int -> Build (BenchResult result)
runBenchmark (Benchmark name setup cmd check) i
 = do   setup 
        (secs, x)       <- timeBuild cmd
        r               <- check x
        return  $ BenchResult name i secs r


-- | Run a benchmark the given number of times.
iterateBenchmark :: Int -> Benchmark result -> Build [BenchResult result]
iterateBenchmark 0 _ = return []
iterateBenchmark count bench
 = do   result  <- runBenchmark bench count
        rest    <- iterateBenchmark (count - 1) bench
        return  $ result : rest


-- | Run a command, returning its elapsed time.
timeBuild :: Build a -> Build (Seconds, a) 
timeBuild cmd
 = do   start     <- io $ getCurrentTime
        result    <- cmd
        finish    <- io $ getCurrentTime
        let time  = fromRational $ toRational $ diffUTCTime finish start
        return (Seconds time, result)

