-- | Querying a darcs repository
--

module BuildBox.Command.Darcs (

  EmailAddress, DarcsPath, DarcsPatch(..),
  patchesAfter, patchesLast

) where

-- standard libraries
import Data.Time
import Data.Maybe
import System.Locale
import qualified Data.Sequence          as Seq
import qualified Data.ByteString.Char8  as B

-- friends
import BuildBox.Build
import BuildBox.Command.System
import qualified BuildBox.Data.Log      as Log


type DarcsPath    = String
type EmailAddress = String

data DarcsPatch = DarcsPatch
  {
    darcsTimestamp :: LocalTime
  , darcsAuthor    :: EmailAddress
  , darcsComment   :: Log.Log
  }

instance Show DarcsPatch where
  show (DarcsPatch time author desc) =
    formatTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" time
    ++ "  " ++ author
    ++ "\n" ++ Log.toString desc


-- | Retrieve the last N patches from the repository. If no repository is given,
--   the current directory is used.
--
patchesLast :: Maybe DarcsPath -> Int -> Build [DarcsPatch]
patchesLast repo n =
  darcs $ "darcs changes --last=" ++ show n
                    ++ " --repo=" ++ fromMaybe "." repo

-- | Retrieve all patches that were submitted to the repository after the given
--   time. If no repository is given, the current working directory is used.
--
patchesAfter :: Maybe DarcsPath -> LocalTime -> Build [DarcsPatch]
patchesAfter repo time =
  darcs $ "darcs changes --matches='date \"after " ++ show time ++ "\"'"
                    ++ " --repo=" ++ fromMaybe "." repo


-- Execute the given darcs command string and split the stdout into a series of
-- patches.
--
darcs :: String -> Build [DarcsPatch]
darcs cmd = do
  (status, logOut, logErr) <- systemTeeLog False cmd Log.empty
  case status of
    ExitSuccess -> return $ splitPatches logOut
    _           -> throw  $ ErrorSystemCmdFailed cmd status logOut logErr

splitPatches :: Log.Log -> [DarcsPatch]
splitPatches l
  | Seq.null l = []
  | otherwise  = let (h,t) = Seq.breakl B.null l
                 in  patch h : splitPatches (Seq.dropWhileL B.null t)
  where
    patch p =
      let toks          = words . B.unpack $ Seq.index p 0
          (time,author) = splitAt 6 toks
      in
      DarcsPatch
        {
          darcsTimestamp = readTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" (unwords time)
        , darcsAuthor    = unwords author
        , darcsComment   = Seq.drop 1 p
        }

