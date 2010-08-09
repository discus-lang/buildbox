{-# LANGUAGE ScopedTypeVariables #-}

module Args
	( BuildArg(..)
	, buildArgs)
where
import System.Console.ParseArgs


data BuildArg
	= ArgTmpDir
	| ArgDoBuild
	| ArgDoTest
	deriving (Eq, Ord, Show)


buildArgs :: [Arg BuildArg]
 = 	[ Arg 	{ argIndex	= ArgTmpDir
		, argAbbr	= Just 'd'
		, argName	= Just "dir"
		, argData	= argDataRequired "dir" ArgtypeString 
		, argDesc	= "Scratch dir to do the build in." }

	  -- Download the latest version and build it.
	, Arg	{ argIndex	= ArgDoBuild
		, argAbbr	= Just 'b'
		, argName	= Just "build"
		, argData	= Nothing
		, argDesc	= "Download the latest version and build it." }
		
	  -- Run regression tests.
	, Arg	{ argIndex	= ArgDoTest
		, argAbbr	= Just 't'
		, argName	= Just "test"
		, argData	= Nothing
		, argDesc	= "Run regression tests." }
	]