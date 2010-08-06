
import BuildBox
import Control.Monad

scratchDir	= "wibble"

main 
 = do	result	<- runBuildAndPrintResult build
	return ()
	
	
-- | Run the complete nightly build.
build
 = do	outLine
	outLn "Repa Nightly Build\n"
	
	platform	<- getHostPlatform
	versionGHC	<- getVersionGHC
	versionGCC	<- getVersionGCC

	outLn $ pprPlatform platform	
	outLn $ "  GHC version: " ++ versionGHC
	outLn $ "  GCC version: " ++ versionGCC
	
	outLine
	outBlank
	buildRepaIn scratchDir
	testRepaIn scratchDir


-- Building ---------------------------------------------------------------------------------------	
-- | Download the Repa package from code.haskell.org,
--   build it and register with current compiler.
buildRepaIn scratchDir
 = do	outCheckOk "* Checking Google is reachable"
	 $ HostReachable "www.google.com"

	outCheckOk "* Checking code.haskell.org is reachable"
	 $ HostReachable "code.haskell.org"
	
	outCheckOk "* Checking code.haskell.org web server is up"
	 $ UrlGettable "http://code.haskell.org"
	
	out "\n"
	inDir scratchDir
	 $ do	outLn "* Getting Darcs Package"
		system "darcs get http://code.haskell.org/repa/repa-head"
		
		outLn "\n"
		inDir "repa-head"
		 $ do	outLn "* Building Packages"
			system "make"


-- Testing ----------------------------------------------------------------------------------------

benchmarks
 = 	-- mmult
	[ let	mmult 	= "repa-examples/dist/build/repa-mmult/repa-mmult"
	  in	Benchmark
			"mmult"
			(test $ HasExecutable mmult)
			(systemNull $ mmult ++ " -random 1024 1024 -random 1024 1024 +RTS -N4 -qg")
			(return True)
			Nothing
	
	-- laplace
	, let	laplace = "repa-examples/dist/build/repa-laplace/repa-laplace"
		input	= "repa-examples/Laplace/data/pls-400x400.bmp"
		inputgz	= input ++ ".gz"

	  in  	Benchmark
		 	"laplace"
			(do	test $ HasExecutable laplace
				whenM (test $ HasFile inputgz)
				 $ system $ "gzip -d " ++ inputgz
				test $ HasFile input)								

			(systemNull $ laplace ++ " 1000 " ++ input ++ " out-laplace.bmp +RTS -N4 -qg")
			(return True)
			Nothing

	-- fft2d-highpass
	, let	fft2d	= "repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		input	= "repa-examples/FFT/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	  in	Benchmark 
			"fft2d-highpass"
			(do	test $ HasExecutable fft2d
				whenM (test $ HasFile inputgz)
				 $ system $ "gzip -d " ++ inputgz
				test $ HasFile input)

			(systemNull $ fft2d ++ " 1 " ++ input ++ " out-fft2d.bmp +RTS -N4 -qg")
			(return True)
			Nothing

	-- fft3d-highpass
	, let	fft3d	= "repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass"
	  in	Benchmark
			"fft3d-highpass"
			(test $ HasExecutable fft3d)
			(systemNull $ fft3d ++ " 128 " ++ " out-fft3d-slice +RTS -N4 -qg")
			(return True)
			Nothing
			
	]


testRepaIn scratchDir
 = inDir (scratchDir ++ "/repa-head")
 $ do	mapM_ runBenchmarkSingle benchmarks


