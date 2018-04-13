{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

-- Don't warn about Data.Monoid import in GHC 8.2 -> 8.4 transition.
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Pretty printing utils.
module BuildBox.Pretty
        ( Pretty(..)
        , Text
        , (%), (%%), empty
        , char, string, text
        , vcat, vsep
        , hcat, hsep
        , parens, braces, brackets, angles
        , indents
        , padRc, padR
        , padLc, padL
        , pprEngDouble
        , pprEngInteger)
where
import Text.Printf
import Control.Monad
import Data.Text                (Text)
import Data.Time
import Data.Monoid
import Data.List
import qualified Data.Text      as T


-- Pretty ---------------------------------------------------------------------
class Pretty a where
        ppr :: a -> Text


-- Basic Combinators ----------------------------------------------------------
-- | An empty text string.
empty :: Text
empty = string " "


-- | Append two text strings.
(%) :: Text -> Text -> Text
(%) t1 t2 = t1 <> t2


-- | Append two text strings separated by a space.
(%%) :: Text -> Text -> Text
(%%) t1 t2 = t1 <> string " " <> t2


-- | Convert a single Char to text.
char :: Char -> Text
char c  = T.pack [c]


-- | Convert a String to text.
string :: String -> Text
string s = T.pack s


-- | Convert a Text to Text (id).
text :: Text -> Text
text t  = t


-- | Concatenate a list of text.
hcat    :: [Text] -> Text
hcat    = mconcat


-- | Concatenate a list of text, with spaces in between.
hsep    :: [Text] -> Text
hsep ts = mconcat $ intersperse (string " ") ts


-- | Concatenate a list of text vertically.
vcat    :: [Text] -> Text
vcat ts = mconcat $ intersperse (string "\n") ts


-- | Concatenate a list of text vertically, with blank lines in between.
vsep    :: [Text] -> Text
vsep ts = mconcat $ intersperse (string "\n\n") ts


-- | Wrap a text thing in round parens.
parens  :: Text -> Text
parens tx       = string "(" % tx % string ")"


-- | Wrap a text thing in round parens.
braces  :: Text -> Text
braces tx       = string "{" % tx % string "}"


-- | Wrap a text thing in round parens.
brackets  :: Text -> Text
brackets tx     = string "[" % tx % string "]"


-- | Wrap a text thing in round parens.
angles  :: Text -> Text
angles tx       = string "<" % tx % string ">"


-- | Indent some text by the given number of characters.
indents :: Int -> [Text] -> Text
indents n ts
        = mconcat [ string (replicate n ' ') % t | t <- ts ]


-- Basic Instances ------------------------------------------------------------
instance Pretty UTCTime where
        ppr     = T.pack . show

instance Pretty Text where
        ppr     = id

instance Pretty String where
        ppr     = T.pack

instance Pretty Int where
        ppr     = T.pack . show

instance Pretty Integer where
        ppr     = T.pack . show

instance Pretty Char where
        ppr     = T.pack . show


-- | Right justify a doc, padding with a given character.
padRc :: Int -> Char -> Text -> Text
padRc n c tx
 = (string $ replicate (n - length (T.unpack tx)) c) <> tx


-- | Right justify a string with spaces.
padR :: Int -> Text -> Text
padR n str
 = padRc n ' ' str


-- | Left justify a string, padding with a given character.
padLc :: Int -> Char -> Text -> Text
padLc n c tx
 = tx <> (string $ replicate (n - length (T.unpack tx)) c)


-- | Left justify a string with spaces.
padL :: Int -> Text -> Text
padL n str
 = padLc n ' ' str


-- Engineering Numbers --------------------------------------------------------
-- | Like `pprEngDouble` but don't display fractional part when the value
--   is < 1000.  Good for units where fractional values might not make sense
--   (like bytes).
pprEngInteger :: String -> Integer -> Maybe Text
pprEngInteger unit k
    | k < 0      = fmap (string "-" <>) $ pprEngInteger unit (-k)
    | k > 1000   = pprEngDouble unit (fromRational $ toRational k)
    | otherwise  = Just $ string $ printf "%5d%s " k unit


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
pprEngDouble :: String -> Double -> Maybe Text
pprEngDouble unit k
    | k < 0      = liftM (string "-" <>) $ pprEngDouble unit (-k)
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
    | otherwise  = Just $ string $ printf "%5.0f%s " k unit
     where
           with (t :: Double) (u :: String)
                | t >= 1e3  = string $ printf "%.0f%s" t u
                | t >= 1e2  = string $ printf "%.1f%s" t u
                | t >= 1e1  = string $ printf "%.2f%s" t u
                | otherwise = string $ printf "%.3f%s" t u
