
module BuildBox.Pretty
	( pprPSecTime
	, pprFloatTime)
where
	


pprFloatTime :: Float -> String
pprFloatTime stime
  = let	psecs	= truncate (stime * 10^12)
    in	pprPSecTime psecs


pprPSecTime :: Integer -> String
pprPSecTime psecs
  = show (psecs `div` 10^(12::Integer)) 
		++ "." 
		++ (take 3 $ padRc 12 '0' $ show $ psecs `mod` 10^(12::Integer))

 
padRc :: Int -> Char -> String -> String
padRc n c str
	= replicate (n - length str) c ++ str