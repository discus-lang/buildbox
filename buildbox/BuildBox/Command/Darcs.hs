-- | Querying a darcs repository
--

module BuildBox.Command.Darcs (

  EmailAddress, DarcsPath, DarcsPatch(..),
  changes, changesN, changesAfter

) where

-- standard libraries
import Data.Time
import Data.Maybe
import qualified Data.Sequence          as Seq
import qualified Data.Text              as Text

-- friends
import BuildBox.Build
import BuildBox.Command.System
import qualified BuildBox.Data.Log      as Log


type DarcsPath    = String
type EmailAddress = String

data DarcsPatch = DarcsPatch
  {
    darcsTimestamp :: LocalTime         -- TLM: use UTC instead?
  , darcsAuthor    :: EmailAddress
  , darcsComment   :: Log.Log
  }

instance Show DarcsPatch where
  show (DarcsPatch time author desc) =
    formatTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" time
    ++ "  " ++ author
    ++ "\n" ++ Log.toString desc


-- | List all patches in the given repository. If no repository is given, the
--   current working directory is used.
--
changes :: Maybe DarcsPath -> Build [DarcsPatch]
changes = darcs . ("darcs changes --repo=" ++) . fromMaybe "."


-- The following are more specific invocations of the "darcs changes" command,
-- which may be faster when interacting with very large repositories or over a
-- slow network.
--

-- | Retrieve the last N changes from the repository
changesN :: Maybe DarcsPath -> Int -> Build [DarcsPatch]
changesN repo n =
  darcs $ "darcs changes --last=" ++ show n
                    ++ " --repo=" ++ fromMaybe "." repo

-- | Retrieve all patches submitted to the repository after the given time
changesAfter :: Maybe DarcsPath -> LocalTime -> Build [DarcsPatch]
changesAfter repo time =
  darcs $ "darcs changes --matches='date \"after " ++ show time ++ "\"'"
                    ++ " --repo=" ++ fromMaybe "." repo


-- Execute the given darcs command string and split the stdout into a series of
-- patches
darcs :: String -> Build [DarcsPatch]
darcs cmd = do
  (status, logOut, logErr) <- systemTeeLog False cmd Log.empty
  case status of
    ExitSuccess -> return $ splitPatches logOut
    _           -> throw  $ ErrorSystemCmdFailed cmd status logOut logErr

splitPatches :: Log.Log -> [DarcsPatch]
splitPatches l
  | Seq.null l = []
  | otherwise  = let (h,t) = Seq.breakl Text.null l
                 in  patch h : splitPatches (Seq.dropWhileL Text.null t)
  where
    patch p =
      let toks          = words . Text.unpack $ Seq.index p 0
          (time,author) = splitAt 6 toks
      in
      DarcsPatch
        {
          darcsTimestamp = parseTimeOrError True 
                                defaultTimeLocale
                                "%a %b %e %H:%M:%S %Z %Y"
                                (unwords time)
        , darcsAuthor    = unwords author
        , darcsComment   = Seq.drop 1 p
        }

