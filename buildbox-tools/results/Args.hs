
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
	| ArgCompare
	| ArgSummarise
	| ArgMerge
	| ArgAccept
	| ArgAdvance
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

	, Arg	{ argIndex	= ArgCompare 
		, argAbbr	= Nothing
		, argName	= Just "compare"
		, argData	= Nothing
		, argDesc	= "Compare two test results files." }

	, Arg	{ argIndex	= ArgSummarise
		, argAbbr	= Nothing
		, argName	= Just "summarise"
		, argData	= argDataOptional "swing" ArgtypeDouble
		, argDesc	= " ... but only return results that have swung by at least this fraction (eg 0.1)" }

	, Arg	{ argIndex	= ArgMerge
		, argAbbr	= Nothing
		, argName	= Just "merge"
		, argData	= Nothing
		, argDesc	= "Merge several test results files." }

	, Arg	{ argIndex	= ArgAccept
		, argAbbr	= Nothing
		, argName	= Just "accept"
		, argData	= argDataOptional "name" ArgtypeString
		, argDesc	= "Accept the most recent run of a benchmark." }

	, Arg	{ argIndex	= ArgAdvance
		, argAbbr	= Nothing
		, argName	= Just "advance"
		, argData	= argDataOptional "swing" ArgtypeDouble
		, argDesc	= "Advance winners who have an aspect that reduced by this fraction (eg 0.1)" }
	]	