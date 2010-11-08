

import BuildBox

run1 =	[ Time TotalWall `secs`  10
	, Used HeapMax   `bytes` 10000]
	
run2 =	[ Time TotalWall `secs`  12
	, Used HeapMax   `bytes` 10200]

run3 =	[ Time TotalWall `secs`  15
	, Used HeapMax   `bytes` 9000]


benchResult
	= BenchResult
	{ benchResultName	= "someTest"
	, benchResultRuns	
	    = 	[ BenchRunResult 1 run1
		, BenchRunResult 2 run2
		, BenchRunResult 3 run3 ] }
		
	
