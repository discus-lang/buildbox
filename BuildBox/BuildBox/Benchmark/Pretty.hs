{-# LANGUAGE PatternGuards #-}

-- | Pretty printing benchmark results.
module BuildBox.Benchmark.Pretty
	( pprBenchResultAspectHeader
	, pprBenchResultAspect)
where
import BuildBox.Benchmark.Base
import BuildBox.Benchmark.TimeAspect
import BuildBox.Pretty

-- | Header to use when pretty printing benchmark results.
pprBenchResultAspectHeader :: Doc
pprBenchResultAspectHeader 
	=  vcat
	[ text "    aspect       min    ref%   avg    ref%   max    ref%   spread% "
	, text "    ---------    -----------   -----------   -----------   ------- " ]


-- | Pretty print an aspect of a benchmark result.
--   If the given aspect does not exist in the result then `Nothing`.
pprBenchResultAspect 
	:: TimeAspect 			-- ^ Aspect of the result to print.
	-> Maybe BenchResult		-- ^ Optional prior result for comparison.
	-> BenchResult			-- ^ The result to print.
	-> Maybe Doc

pprBenchResultAspect aspect prior result
 	| Just (tmin,  tavg,  tmax)	<- takeMinAvgMaxOfBenchResult aspect result
	, spread			<- tmax - tmin
	, spreadPercent			<- (floor $ (spread / tavg) * 100) :: Integer
	, Just result'			<- prior
	, Just (tmin', tavg', tmax')	<- takeMinAvgMaxOfBenchResult aspect result'
	= Just	$   text "    "
		<>  padL 10 (ppr aspect)
		<+> (padR 13 $ pprFloatRef tmin tmin')
		<+> (padR 13 $ pprFloatRef tavg tavg')
		<+> (padR 13 $ pprFloatRef tmax tmax')
		<+> (padR 8  $ ppr spreadPercent)

 	| Just (tmin, tavg, tmax)	<- takeMinAvgMaxOfBenchResult aspect result
	, spread			<- tmax - tmin
	, spreadPercent			<- (floor $ (spread / tavg) * 100) :: Integer
	= Just	$   text "    "
		<>  padL 10 (ppr aspect)
		<+> (padR 13 $ (pprFloatTime tmin <> text "     "))
		<+> (padR 13 $ (pprFloatTime tavg <> text "     "))
		<+> (padR 13 $ (pprFloatTime tmax <> text "     "))
		<+> (padR 8 $ ppr spreadPercent)
	
	| otherwise
	= Nothing	
