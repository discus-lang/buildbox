{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

import Args
import BuildBox
import BuildBox.FileFormat.BuildResults
import System.Console.ParseArgs	hiding (args)


main :: IO ()
main 
 = do	args	<- parseArgsIO ArgsTrailing resultsArgs
	mainWithArgs args


mainWithArgs :: Args ResultsArg -> IO ()
mainWithArgs args

	-- Print usage help
	| gotArg args ArgHelp
	= usageError args ""

	-- Dump a results file.
	| Just fileName	<- getArg args ArgDump
	, []		<- argsRest args
	= do	contents	<- readFile fileName
		let results	=  (read contents) :: BuildResults
		putStrLn $ render $ ppr results

	-- Compare two results files.
	| gotArg args ArgCompare
	= do	let fileNames	= argsRest args
		contentss	<- mapM readFile fileNames

		let (results :: [BuildResults])
			= map read contentss
		
		let [baseline, current] 
			= map buildResultBench results

		putStrLn $ render $ pprComparisons baseline current


	| otherwise
	= usageError args "Nothing to do...\n"
