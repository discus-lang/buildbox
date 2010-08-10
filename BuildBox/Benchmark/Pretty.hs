
module BuildBox.Benchmark.Pretty
	( pprBenchResultAspectHeader
	, pprBenchResultAspect)
where
import BuildBox.Benchmark.Base
import BuildBox.Benchmark.TimeAspect
import BuildBox.Pretty

-- | Header to use when pretty printing benchmark results.
pprBenchResultAspectHeader :: String
pprBenchResultAspectHeader 
	=  "                   "
	++ "    "
	++ " min   ref%  "
	++ " avg   ref%  "
	++ " max   ref%  "
	++ "  spread% "


-- | Pretty print an aspect of a benchmark result.
pprBenchResultAspect :: TimeAspect -> Maybe BenchResult ->  BenchResult -> Maybe String
pprBenchResultAspect aspect prior result
 	| Just (min,  avg,  max)	<- takeMinAvgMaxOfBenchResult aspect result
	, spread			<- max - min
	, spreadPercent			<- floor $ (spread / avg) * 100
	, Just result'			<- prior
	, Just (min', avg', max')	<- takeMinAvgMaxOfBenchResult aspect result'
	= Just
	$	"    "
		++ padL 15 (pprTimeAspect aspect)
		++ "    "
		++ (padR 12 $ pprFloatTimeAgainst min min')
		++ " "
		++ (padR 12 $ pprFloatTimeAgainst avg avg')
		++ " "
		++ (padR 12 $ pprFloatTimeAgainst max max')
		++ " "
		++ (padR 8 $ show spreadPercent)


 	| Just (min, avg, max)		<- takeMinAvgMaxOfBenchResult aspect result
	, spread			<- max - min
	= Just
	$	"    "
		++ padL 15 (pprTimeAspect aspect)
		++ "    "
		++ (padR 12 $ pprFloatTime min)
		++ " "
		++ (padR 12 $ pprFloatTime avg)
		++ " "
		++ (padR 12 $ pprFloatTime max)
		++ " "
		++ (padR 12 $ pprFloatTime spread)
	
	
	| otherwise
	= Nothing	
