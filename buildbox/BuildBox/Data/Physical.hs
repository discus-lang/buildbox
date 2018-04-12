
module BuildBox.Data.Physical
        ( Seconds       (..)
        , Bytes         (..))
where
import BuildBox.Data.Dividable
import BuildBox.Pretty
import Data.Maybe
import Text.PrettyPrint


-- | Seconds of time, pretty printed in engineering format.
data Seconds    = Seconds Double
                deriving (Read, Show, Ord, Eq)

instance Real Seconds where
        toRational (Seconds s1)         = toRational s1

instance Dividable Seconds where
        divide (Seconds s1) (Seconds s2) = Seconds (s1 / s2)

instance Num Seconds where
        (+) (Seconds f1) (Seconds f2)   = Seconds (f1 + f2)
        (-) (Seconds f1) (Seconds f2)   = Seconds (f1 - f2)
        (*) (Seconds f1) (Seconds f2)   = Seconds (f1 * f2)
        abs (Seconds f1)                = Seconds (abs f1)
        signum (Seconds f1)             = Seconds (signum f1)
        fromInteger i                   = Seconds (fromInteger i)

instance Pretty Seconds where
        ppr (Seconds f)
                = fromMaybe (text (show f))
                $ pprEngDouble "s" f


-- | Bytes of data, pretty printed in engineering format.
data Bytes      = Bytes   Integer
                deriving (Read, Show, Ord, Eq)

instance Real Bytes where
        toRational (Bytes b1)           = toRational b1

instance Dividable Bytes where
        divide (Bytes s1) (Bytes s2)    = Bytes (s1 `div` s2)

instance Num Bytes where
        (+) (Bytes f1) (Bytes f2)       = Bytes (f1 + f2)
        (-) (Bytes f1) (Bytes f2)       = Bytes (f1 - f2)
        (*) (Bytes f1) (Bytes f2)       = Bytes (f1 * f2)
        abs (Bytes f1)                  = Bytes (abs f1)
        signum (Bytes f1)               = Bytes (signum f1)
        fromInteger i                   = Bytes (fromInteger i)

instance Pretty Bytes where
        ppr (Bytes b)
                = fromMaybe (text (show b))
                $ pprEngInteger "B" b
