{-# LANGUAGE ScopedTypeVariables #-}

module Args
	( BuildArg(..)
	, buildArgs)
where
import System.Console.ParseArgs


-- Command line args for the buildbot.
data BuildArg
	= ArgVerbose
	| ArgTmpDir
	| ArgDoDump
	| ArgDoBuild
	| ArgDoTest
	| ArgTestIterations
	| ArgWriteResults
	deriving (Eq, Ord, Show)


buildArgs :: [Arg BuildArg]
 = 	[ Arg	{ argIndex	= ArgVerbose
		, argAbbr	= Just 'v'
		, argName	= Just "verbose"
		, argData	= Nothing
		, argDesc	= "Verbose logging of build commands" }

	, Arg	{ argIndex	= ArgDoDump
		, argAbbr	= Nothing
		, argName	= Just "dump"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Dump a results file in human readable format" }

	, Arg 	{ argIndex	= ArgTmpDir
		, argAbbr	= Just 'd'
		, argName	= Just "dir"
		, argData	= argDataOptional "dir" ArgtypeString 
		, argDesc	= "Scratch dir to do the build in." }

	, Arg	{ argIndex	= ArgDoBuild
		, argAbbr	= Just 'b'
		, argName	= Just "build"
		, argData	= Nothing
		, argDesc	= "Download the latest version and build it." }
		
	, Arg	{ argIndex	= ArgDoTest
		, argAbbr	= Just 't'
		, argName	= Just "test"
		, argData	= Nothing
		, argDesc	= "Run regression tests." }

	, Arg	{ argIndex	= ArgTestIterations
		, argAbbr	= Just 'i'
		, argName	= Just "iterations"
		, argData	= argDataDefaulted "iters" ArgtypeInt 1
		, argDesc	= "Number of times to run each benchmark." }
		
	, Arg	{ argIndex	= ArgWriteResults
		, argAbbr	= Just 'w'
		, argName	= Just "write"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Write results to this file." }
		
	]