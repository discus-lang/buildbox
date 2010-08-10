{-# LANGUAGE ScopedTypeVariables #-}

module Args
	( BuildArg(..)
	, buildArgs)
where
import System.Console.ParseArgs


-- Command line args for the buildbot.
data BuildArg
	= ArgHelp
	| ArgVerbose
	| ArgTmpDir
	| ArgDoDump
	| ArgDoCompare
	| ArgDoBuild
	| ArgDoTest
	| ArgTestIterations
	| ArgWriteResults
	| ArgWithResults
	deriving (Eq, Ord, Show)


buildArgs :: [Arg BuildArg]
 = 	[ Arg	{ argIndex	= ArgHelp
		, argAbbr	= Just 'h'
		, argName	= Just "help"
		, argData	= Nothing
		, argDesc	= "Print this usage help." }

	, Arg	{ argIndex	= ArgDoDump
		, argAbbr	= Nothing
		, argName	= Just "dump"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Dump a results file in human readable format." }

	, Arg	{ argIndex	= ArgDoCompare 
		, argAbbr	= Nothing
		, argName	= Just "compare"
		, argData	= Nothing
		, argDesc	= "Compare two results files." }

	, Arg	{ argIndex	= ArgDoBuild
		, argAbbr	= Nothing
		, argName	= Just "build"
		, argData	= Nothing
		, argDesc	= "Download the latest version and build it." }
		
	, Arg	{ argIndex	= ArgDoTest
		, argAbbr	= Nothing
		, argName	= Just "test"
		, argData	= Nothing
		, argDesc	= "Run regression tests." }

	, Arg	{ argIndex	= ArgVerbose
		, argAbbr	= Just 'v'
		, argName	= Just "verbose"
		, argData	= Nothing
		, argDesc	= "Verbose logging of build commands." }

	, Arg 	{ argIndex	= ArgTmpDir
		, argAbbr	= Just 'd'
		, argName	= Just "dir"
		, argData	= argDataOptional "dir" ArgtypeString 
		, argDesc	= "(req. for build and test modes) Scratch dir to do the build in." }

	, Arg	{ argIndex	= ArgTestIterations
		, argAbbr	= Just 'i'
		, argName	= Just "iterations"
		, argData	= argDataDefaulted "int" ArgtypeInt 1
		, argDesc	= "(opt. for test mode) Number of times to run each benchmark." }
		
	, Arg	{ argIndex	= ArgWriteResults
		, argAbbr	= Just 'w'
		, argName	= Just "write"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test mode) Write results to this file." }
		
	, Arg	{ argIndex	= ArgWithResults
		, argAbbr	= Just 't'
		, argName	= Just "with"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test mode) Print running comparison with results in this file." }
	]