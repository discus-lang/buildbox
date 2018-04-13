
module BuildBox.Data.Range
        ( Range (..)
        , makeRange
        , flattenRange)
where
import BuildBox.Pretty
import BuildBox.Data.Dividable


-- | A range extracted from many-valued data.
data Range a
        = Range
        { rangeMin      :: a
        , rangeAvg      :: a
        , rangeMax      :: a }
        deriving (Read, Show)


instance Pretty a => Pretty (Range a) where
 ppr (Range mi av mx)
        =  ppr mi %% string "/"
        %% ppr av %% string "/"
        %% ppr mx


instance Functor Range where
 fmap f (Range mi av mx)
        = Range (f mi) (f av) (f mx)


-- | Make statistics from a list of values.
makeRange :: (Real a, Dividable a) => [a] -> Range a
makeRange xs
        = Range (minimum xs)
                (sum xs `divide` (fromIntegral $ length xs))
                (maximum xs)


-- | Flatten a `Range` into a list of its min, avg and max values.
flattenRange :: Range a -> [a]
flattenRange (Range mi av mx)
        = [mi, av, mx]
