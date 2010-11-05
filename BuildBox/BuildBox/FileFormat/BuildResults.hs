
module BuildBox.FileFormat.BuildResults
	( BuildResults(..)
	, mergeResults)
where
import BuildBox.Time
import BuildBox.Benchmark
import BuildBox.Command.Environment
import BuildBox.Pretty
import BuildBox.Aspect
import Data.List


-- | A simple build results file format.
data BuildResults
	= BuildResults
	{ buildResultTime		:: UTCTime
	, buildResultEnvironment	:: Environment
	, buildResultBench		:: [BenchResult Single] }
	deriving (Show, Read)

instance Pretty BuildResults where
 ppr results
	= hang (ppr "BuildResults") 2 $ vcat
	[ ppr "time: " <> (ppr $ buildResultTime results)
	, ppr $ buildResultEnvironment results
	, ppr ""
	, vcat 	$ punctuate (ppr "\n") 
		$ map ppr 
		$ buildResultBench results ]


-- | Merge some BuildResults.
--   If we have data for a named benchmark in multiple `BuildResults`,
--   then we take the first one in the list.
--   The resultTime and environment is taken from the last `BuildResults`,
--   in the list.
mergeResults :: [BuildResults] -> BuildResults
mergeResults results
 = let	
	-- All the available benchResults from all files.
	benchResults
		= concatMap buildResultBench results

	-- Get a the names of all the available benchmarks.
	benchNames  
		= sort $ nub
		$ map benchResultName
		$ concatMap buildResultBench results 

	-- Merge all the results
	Just newBenchResults
		= sequence 
		$ [ find (\br -> benchResultName br == name) benchResults	
				| name <- benchNames]
			
	-- Use the timestamp and environment from the last one.
	(lastResults : _) = reverse results
		
   in BuildResults
		{ buildResultTime	 = buildResultTime lastResults
		, buildResultEnvironment = buildResultEnvironment lastResults
		, buildResultBench	 = newBenchResults }
