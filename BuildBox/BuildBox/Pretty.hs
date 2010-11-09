{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances, IncoherentInstances #-}

-- | Pretty printing utils.
module BuildBox.Pretty
	( module Text.PrettyPrint
	, Pretty(..)
	, pprPSecTime
	, pprFloatTime
	, pprFloatSR
	, pprFloatRef
	, padRc, padR
	, padLc, padL
	, blank
	, pprEngDouble
	, pprEngInteger)
where
import Text.PrettyPrint
import Text.Printf
import Data.Time
import Control.Monad

-- Things that can be pretty printed
class Pretty a where
 	ppr :: a -> Doc

-- Basic instances
instance Pretty Doc where
	ppr = id
	
instance Pretty Float where
	ppr = text . show

instance Pretty Int where
	ppr = int
	
instance Pretty Integer where
	ppr = text . show

instance Pretty UTCTime where
	ppr = text . show
	
instance Pretty a => Pretty [a] where
	ppr xx 
		= lbrack <> (hcat $ punctuate (text ", ") (map ppr xx)) <> rbrack

instance Pretty String where
	ppr = text

-- To handle type defaulting
ten12i :: Integer
ten12i = 10^(12 :: Integer)


-- | Print a number of picoseconds as a time.
pprPSecTime :: Integer -> Doc
pprPSecTime psecs
  	=  text (show (psecs `quot` ten12i))
	<> text "." 
 	<> (text $ (take 3 $ render $ padRc 12 '0' $ text $ show $ psecs `rem` ten12i))


-- | Print a float number of seconds as a time.
pprFloatTime :: Float -> Doc
pprFloatTime stime
  = let (secs :: Integer, frac :: Float)	
			= properFraction stime

	msecs		= frac * 1000
    in	text (show secs) 
	 <> text "."
	 <> (padRc 3 '0' $ text $ show $ ((round $ msecs) :: Integer) )


-- | Pretty print a signed float, with a percentage change relative to a reference figure.
--   Comes out like @0.235( +5)@ for a +5 percent swing.
pprFloatRef :: Float -> Float -> Doc
pprFloatRef stime stimeRef 
 = let	diff		= ((stime - stimeRef) / stimeRef )*100
   in	pprFloatTime stime
	 <> parens (padR 4 $ pprFloatSR diff)


-- | Print a float number of seconds, rounding it and, prefixing with @+@ or @-@ appropriately.
pprFloatSR :: Float -> Doc
pprFloatSR p
	| p == 0
	= text "----"

 	| p > 0
	= text "+" <> (ppr $ (round p :: Integer))
	
	| otherwise
	= text "-" <> (ppr $ (round (negate p) :: Integer))


-- | Right justify a doc, padding with a given character.
padRc :: Int -> Char -> Doc -> Doc
padRc n c str
	= (text $ replicate (n - length (render str)) c) <> str
	

-- | Right justify a string with spaces.
padR :: Int -> Doc -> Doc
padR n str	= padRc n ' ' str


-- | Left justify a string, padding with a given character.
padLc :: Int -> Char -> Doc -> Doc
padLc n c str
	= str <> (text $ replicate (n - length (render str)) c)


-- | Left justify a string with spaces.
padL :: Int -> Doc -> Doc
padL n str	= padLc n ' ' str

-- | Blank text. This is different different from `empty` because it comes out a a newline when used in a `vcat`.
blank :: Doc
blank = ppr ""


-- | Like `pprEngDouble` but don't display fractional part when the value is < 1000.
pprEngInteger :: String -> Integer -> Maybe String
pprEngInteger unit k
    | k < 0	 = liftM ("-" ++) $ pprEngInteger unit (-k)
    | k > 1000	 = pprEngDouble unit (fromRational $ toRational k)
    | otherwise  = Just $ printf "%5d%s " k unit


-- | Pretty print an engineering value, to 4 significant figures.
--   Valid range is  -10^24 (y/yocto) to 10^24 (Y/Yotta)
--   Out of range values yield Nothing.
pprEngDouble :: String -> Double -> Maybe String
pprEngDouble unit k
    | k < 0      = liftM ("-" ++) $ pprEngDouble unit (-k)
    | k >= 1e+27 = Nothing
    | k >= 1e+24 = Just $ (k*1e-24) `with` ("Y" ++ unit)
    | k >= 1e+21 = Just $ (k*1e-21) `with` ("Z" ++ unit)
    | k >= 1e+18 = Just $ (k*1e-18) `with` ("E" ++ unit)
    | k >= 1e+15 = Just $ (k*1e-15) `with` ("P" ++ unit)
    | k >= 1e+12 = Just $ (k*1e-12) `with` ("T" ++ unit)
    | k >= 1e+9  = Just $ (k*1e-9)  `with` ("G" ++ unit)
    | k >= 1e+6  = Just $ (k*1e-6)  `with` ("M" ++ unit)
    | k >= 1e+3  = Just $ (k*1e-3)  `with` ("k" ++ unit)
    | k >= 1     = Just $ k         `with` (unit ++ " ")
    | k >= 1e-3  = Just $ (k*1e+3)  `with` ("m" ++ unit)
    | k >= 1e-6  = Just $ (k*1e+6)  `with` ("u" ++ unit)
    | k >= 1e-9  = Just $ (k*1e+9)  `with` ("n" ++ unit)
    | k >= 1e-12 = Just $ (k*1e+12) `with` ("p" ++ unit)
    | k >= 1e-15 = Just $ (k*1e+15) `with` ("f" ++ unit)
    | k >= 1e-18 = Just $ (k*1e+18) `with` ("a" ++ unit)
    | k >= 1e-21 = Just $ (k*1e+21) `with` ("z" ++ unit)
    | k >= 1e-24 = Just $ (k*1e+24) `with` ("y" ++ unit)
    | k >= 1e-27 = Nothing
    | otherwise  = Just $ printf "%5.0f%s " k unit
     where with (t :: Double) (u :: String)
		| t >= 1e3  = printf "%.0f%s" t u
		| t >= 1e2  = printf "%.1f%s" t u
		| t >= 1e1  = printf "%.2f%s" t u
		| otherwise = printf "%.3f%s" t u
