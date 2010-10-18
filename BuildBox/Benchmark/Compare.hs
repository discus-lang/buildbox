
-- | Pretty printing comparisons of benchmark results.
module BuildBox.Benchmark.Compare
	( pprComparison
	, pprComparisons)
where
import BuildBox.Pretty
import BuildBox.Benchmark.Base
import BuildBox.Benchmark.Pretty
import BuildBox.Benchmark.TimeAspect
import Data.Maybe
import Data.List

-- | Pretty print a comparison of all the aspects of these two benchmark results.
--   The first result is the ``baseline'', while the second is the ``current'' one.
--   The numbers from the current results are printed, with a percentage relative to the baseline.
pprComparison :: BenchResult -> BenchResult -> Doc
pprComparison baseline current
 	= vcat
	[ pprBenchResultAspectHeader
	, fromMaybe empty $ pprBenchResultAspect TimeAspectElapsed	 (Just baseline) current
	, fromMaybe empty $ pprBenchResultAspect TimeAspectKernelElapsed (Just baseline) current
	, fromMaybe empty $ pprBenchResultAspect TimeAspectKernelCpu	 (Just baseline) current
	, fromMaybe empty $ pprBenchResultAspect TimeAspectKernelSys	 (Just baseline) current ]


-- | Pretty print a comparison of all the aspects of these two benchmark results.
--   The first result is the ``baseline'' while the second is the ``current'' one.
--   All the numbers from the current results are printed, with a percentage relative to the
--   baseline if there is one. If there is no baseline for a particular result then we still
--   print the current one.
pprComparisons :: [BenchResult] -> [BenchResult] -> Doc
pprComparisons baselines currents
 = let	comparison current
	 = let	mBaseline = find (\b -> benchResultName b == benchResultName current)
			  $ baselines

	   in	vcat
		[ fromMaybe empty $ pprBenchResultAspect TimeAspectElapsed	 mBaseline current
		, fromMaybe empty $ pprBenchResultAspect TimeAspectKernelElapsed mBaseline current
		, fromMaybe empty $ pprBenchResultAspect TimeAspectKernelCpu	 mBaseline current
		, fromMaybe empty $ pprBenchResultAspect TimeAspectKernelSys	 mBaseline current ]

   in	vcat
	[ nest 30 pprBenchResultAspectHeader
	, vcat	$ punctuate (text "\n")
		$ map (\c -> (text (benchResultName c) $$ (nest 30 $ comparison c)))
		$ currents ]


