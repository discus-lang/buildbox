{-# LANGUAGE TypeSynonymInstances #-}

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
	, blank)
where
import Text.PrettyPrint
import Data.Time

-- Things that can be pretty printed
class Pretty a where
 	ppr :: a -> Doc

-- Basic instances
instance Pretty Doc where
	ppr = id

instance Pretty String where
	ppr = text
	
instance Pretty Float where
	ppr = text . show

instance Pretty Int where
	ppr = int
	
instance Pretty Integer where
	ppr = text . show

instance Pretty UTCTime where
	ppr = text . show
	

-- To handle type defaulting
ten12 :: Float
ten12 = 10^(12 :: Integer)

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
  = let (secs, frac)	= properFraction stime
	msecs		= frac * 1000
    in	text (show secs) 
	 <> text "."
	 <> (padRc 3 '0' $ text $ show $ round $ msecs)


-- | Pretty print a signed float, with a percentage change relative to a reference figure.
--   Comes out like @0.235( +5)@ for a +5 percent swing.
pprFloatRef :: Float -> Float -> Doc
pprFloatRef stime stimeRef 
 = let	diff		= ((stime - stimeRef) / stimeRef )*100
   in	pprFloatTime stime
	 <> parens (padR 3 $ pprFloatSR diff)


-- | Print a float number of seconds, rounding it and, prefixing with @+@ or @-@ appropriately.
pprFloatSR :: Float -> Doc
pprFloatSR p
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