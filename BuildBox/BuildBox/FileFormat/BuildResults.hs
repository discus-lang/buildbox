
-- | A simple build results file format.
module BuildBox.FileFormat.BuildResults
	(BuildResults(..))
where
import BuildBox.Time
import BuildBox.Benchmark
import BuildBox.Command.Environment
import BuildBox.Pretty

data BuildResults
	= BuildResults
	{ buildResultTime		:: UTCTime
	, buildResultEnvironment	:: Environment
	, buildResultBench		:: [BenchResult] }
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
