{-# LANGUAGE TypeSynonymInstances #-}

-- | Pretty printing utils.
module BuildBox.Pretty
	( module Text.PrettyPrint
	, Pretty(..)
	, pprPSecTime
	, pprSignedPercentage
	, pprFloatTime
	, pprFloatTimeAgainst
	, padRc, padR
	, padLc, padL )
where
import Text.PrettyPrint
import Data.Time

-- Things that can be pretty printed
class Pretty a where
 	ppr :: a -> Doc

-- Basic instances
instance Pretty String where
	ppr = text
	
instance Pretty Float where
	ppr = text . show

instance Pretty UTCTime where
	ppr = text . show
	

-- | Print a floating point number of seconds as a time.
pprFloatTime :: Float -> String
pprFloatTime stime
  = let	psecs	= truncate (stime * 10^12)
    in	pprPSecTime psecs


-- | Print a floating point number of seconds as a time.
--   Also give a +- percentage relative to a reference figure.
pprFloatTimeAgainst :: Float -> Float -> String
pprFloatTimeAgainst stime stimeRef 
 = let	psecs		= truncate (stime    * 10^12)
	psecsRef	= truncate (stimeRef * 10^12)
	diff		= (1 - (stimeRef / stime))*100

   in	(pprPSecTime psecs)
		++ "(" ++ pprSignedPercentage diff ++ ")"


-- | Pretty print a percentage.
pprSignedPercentage :: Float -> String
pprSignedPercentage p
 	| p > 0
	= "+" ++ padR 3 (show $ floor p)
	
	| otherwise
	= "-" ++ padR 3 (show $ floor (negate p))
	


-- | Print a number of pico seconds as a time.
pprPSecTime :: Integer -> String
pprPSecTime psecs
  = show (psecs `div` 10^(12::Integer)) 
		++ "." 
		++ (take 3 $ padRc 12 '0' $ show $ psecs `mod` 10^(12::Integer))


-- | Right justify a string.
padRc :: Int -> Char -> String -> String
padRc n c str
	= replicate (n - length str) c ++ str
	

-- | Right justify a string with spaces.
padR :: Int -> String -> String
padR n str	= padRc n ' ' str


-- | Left justify a string.
padLc :: Int -> Char -> String -> String
padLc n c str
	= str ++ replicate (n - length str) c


-- | Left justify a string with spaces.
padL :: Int -> String -> String
padL n str	= padLc n ' ' str

