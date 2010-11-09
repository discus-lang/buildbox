

import BuildBox

run1 =	[ Time TotalWall `secs`  10
	, Used HeapMax   `bytes` 10000]
	
run2 =	[ Time TotalWall `secs`  12
	, Used HeapMax   `bytes` 10200]

run3 =	[ Time TotalWall `secs`  15
	, Used HeapMax   `bytes` 9000]

run4 =	[ Time TotalWall `secs`  20
	, Used HeapMax   `bytes` 8000]

run5 =	[ Time TotalWall `secs`  9
	, Used HeapMax   `bytes` 1000]

run6 =	[ Time TotalWall `secs`  8
	, Used HeapMax   `bytes` 5000]


benchResult1
	= BenchResult
	{ benchResultName	= "someTest"
	, benchResultRuns	
	    = 	[ BenchRunResult 1 run1
		, BenchRunResult 2 run2
		, BenchRunResult 3 run3 ] }

benchResult2
	= BenchResult
	{ benchResultName	= "someTest"
	, benchResultRuns	
	    = 	[ BenchRunResult 1 run4
		, BenchRunResult 2 run5
		, BenchRunResult 3 run6 ] }
		
	
