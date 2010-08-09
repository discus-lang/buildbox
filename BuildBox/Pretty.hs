{-# LANGUAGE TypeSynonymInstances #-}

-- | Pretty printing utils.
module BuildBox.Pretty
	( module Text.PrettyPrint
	, Pretty(..)
	, pprPSecTime
	, pprFloatTime
	, padRc, padR
	, padLc, padL )
where
import Text.PrettyPrint

-- Things that can be pretty printed
class Pretty a where
 	ppr :: a -> Doc

instance Pretty String where
	ppr = text
	

-- | Print a floating point number of seconds as a time.
pprFloatTime :: Float -> String
pprFloatTime stime
  = let	psecs	= truncate (stime * 10^12)
    in	pprPSecTime psecs


-- | Print a number of pico seconds as a time.
pprPSecTime :: Integer -> String
pprPSecTime psecs
  = show (psecs `div` 10^(12::Integer)) 
		++ "." 
		++ (take 3 $ padRc 12 '0' $ show $ psecs `mod` 10^(12::Integer))


-- | Right justify a string.
padRc :: Int -> Char -> String -> String
padRc n c str
	= str ++ replicate (n - length str) c
	

-- | Right justify a string with spaces.
padR :: Int -> String -> String
padR n str	= padRc n ' ' str


-- | Left justify a string.
padLc :: Int -> Char -> String -> String
padLc n c str
	= replicate (n - length str) c  ++ str


-- | Left justify a string with spaces.
padL :: Int -> String -> String
padL n str	= padLc n ' ' str

