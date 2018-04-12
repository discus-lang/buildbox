{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Pretty printing utils.
module BuildBox.Pretty
        ( Pretty(..)
        , padRc, padR
        , padLc, padL
        , blank
        , pprEngDouble
        , pprEngInteger
        , renderIndent
        , renderPlain
        , ppr)
where
import Text.Printf
import Text.PrettyPrint.Leijen
import Data.Time
import Control.Monad
import Prelude  hiding ((<>))


-- Basic instances
instance Pretty UTCTime where
        pretty  = text . show

instance Pretty String where
        pretty  = text


ppr :: Pretty a => a -> Doc
ppr = pretty


-- | Render a thing as a string.
renderIndent :: Pretty a => a -> String
renderIndent x
        = displayS (renderPretty 0.4 80 (pretty x)) ""


-- | Render a thing with no indenting.
renderPlain :: Pretty a => a -> String
renderPlain x
        = displayS (renderCompact (pretty x)) ""


-- | Right justify a doc, padding with a given character.
padRc :: Int -> Char -> Doc -> Doc
padRc n c str
        =  (text $ replicate (n - length (renderPlain str)) c) <> str


-- | Right justify a string with spaces.
padR :: Int -> Doc -> Doc
padR n str      = padRc n ' ' str


-- | Left justify a string, padding with a given character.
padLc :: Int -> Char -> Doc -> Doc
padLc n c str
        = str <> (text $ replicate (n - length (displayS (renderPretty 0.4 80 str) "")) c)


-- | Left justify a string with spaces.
padL :: Int -> Doc -> Doc
padL n str      = padLc n ' ' str

-- | Blank text. This is different different from `empty` because it comes out a a
--   newline when used in a `vcat`.
blank :: Doc
blank = pretty ""


-- | Like `pprEngDouble` but don't display fractional part when the value is < 1000.
--   Good for units where fractional values might not make sense (like bytes).
pprEngInteger :: String -> Integer -> Maybe Doc
pprEngInteger unit k
    | k < 0      = liftM (text "-" <>) $ pprEngInteger unit (-k)
    | k > 1000   = pprEngDouble unit (fromRational $ toRational k)
    | otherwise  = Just $ text $ printf "%5d%s " k unit


-- | Pretty print an engineering value, to 4 significant figures.
--   Valid range is  10^(-24) (y\/yocto) to 10^(+24) (Y\/Yotta).
--   Out of range values yield Nothing.
--
--   examples:
--
--   @
--   liftM render $ pprEngDouble \"J\" 102400    ==>   Just \"1.024MJ\"
--   liftM render $ pprEngDouble \"s\" 0.0000123 ==>   Just \"12.30us\"
--   @
--
pprEngDouble :: String -> Double -> Maybe Doc
pprEngDouble unit k
    | k < 0      = liftM (text "-" <>) $ pprEngDouble unit (-k)
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
    | otherwise  = Just $ text $ printf "%5.0f%s " k unit
     where with (t :: Double) (u :: String)
                | t >= 1e3  = text $ printf "%.0f%s" t u
                | t >= 1e2  = text $ printf "%.1f%s" t u
                | t >= 1e1  = text $ printf "%.2f%s" t u
                | otherwise = text $ printf "%.3f%s" t u
