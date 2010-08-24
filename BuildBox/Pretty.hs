{-# LANGUAGE TypeSynonymInstances #-}

-- | Pretty printing utils.
module BuildBox.Pretty
	( module Text.PrettyPrint
	, Pretty(..)
	, pprPSecTime
	, pprFloatTime
	, pprFloatSF
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
  	=  text (show (psecs `div` ten12i))
	<> text "." 
 	<> (text $ (take 3 $ render $ padRc 12 '0' $ text $ show $ psecs `mod` ten12i))


-- | Print a float number of seconds as a time.
pprFloatTime :: Float -> Doc
pprFloatTime stime
  = let	psecs	= truncate (stime * ten12)
    in	pprPSecTime psecs


-- | Pretty print a signed float, with a percentage change relative to a reference figure.
--   Comes out like @0.235( +5)@ for a +5 percent swing.
pprFloatRef :: Float -> Float -> Doc
pprFloatRef stime stimeRef 
 = let	psecs		= truncate (stime    * ten12)
	diff		= (1 - (stimeRef / stime))*100
   in	pprPSecTime psecs
	 <> parens (padR 3 $ pprFloatSF diff)


-- | Print a float number of seconds, flooring it and, prefixing with @+@ or @-@ appropriately.
pprFloatSF :: Float -> Doc
pprFloatSF p
 	| p > 0
	= text "+" <> (ppr $ (floor p :: Integer))
	
	| otherwise
	= text "-" <> (ppr $ (floor (negate p) :: Integer))


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