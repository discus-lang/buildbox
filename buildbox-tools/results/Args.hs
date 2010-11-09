
module Args
	( ResultsArg(..)
	, resultsArgs)
where
import System.Console.ParseArgs

-- | Command line args for the buildbot.
data ResultsArg
	= ArgHelp
	| ArgDump
	| ArgNames
	| ArgMerge
	| ArgCompare
	| ArgSummarise
	deriving (Eq, Ord, Show)


-- | Command line argument definitions.
resultsArgs :: [Arg ResultsArg]
resultsArgs
 = 	[ Arg	{ argIndex	= ArgHelp
		, argAbbr	= Just 'h'
		, argName	= Just "help"
		, argData	= Nothing
		, argDesc	= "Print this usage help." }

	, Arg	{ argIndex	= ArgDump
		, argAbbr	= Nothing
		, argName	= Just "dump"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Dump a benchmark results file in human readable format." }
	
	, Arg	{ argIndex	= ArgNames
		, argAbbr	= Nothing
		, argName	= Just "names"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Print the names of all the benchmarks in a file." }

	, Arg	{ argIndex	= ArgMerge
		, argAbbr	= Nothing
		, argName	= Just "merge"
		, argData	= Nothing
		, argDesc	= "Merge several test results files." }
		
	, Arg	{ argIndex	= ArgCompare 
		, argAbbr	= Nothing
		, argName	= Just "compare"
		, argData	= Nothing
		, argDesc	= "Compare two test results files." }

	, Arg	{ argIndex	= ArgSummarise
		, argAbbr	= Nothing
		, argName	= Just "summarise"
		, argData	= argDataOptional "swing" ArgtypeDouble
		, argDesc	= " ... but only return results that have swung by at least this fraction." }
	]	