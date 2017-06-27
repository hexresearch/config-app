-- | Options to use with Aeson's TH
--
-- Common usage:
--
-- > data PagedList a = PagedList {
-- >   pagedListItems :: ![a] -- ^ Payload
-- > , pagedListPages :: !Word -- ^ Count of available pages
-- > } deriving (Generic, Show)
-- >
-- > $(deriveJSON (exactPrefixOptions "pagedList") ''PagedList)
--
-- It will marshal in/from the following json structure:
--
-- > { "items": [], "pages": 0 }
module System.AesonOptions
  ( defaultOptions
  , defaultNewtypeOptions
  , leaveTagOptions
  , exactPrefixOptions
  , exactPrefixOptionsSnaky
  , A.deriveJSON
  , A.deriveFromJSON
  , A.deriveToJSON
  , Options(..)
  -- * helpers
  , headToLower
  , stripFieldPrefix
  , stripExactPrefix
  , dropPunctuation
  , stripConstructorPrefix
  ) where

import           Data.Aeson.TH (Options (..), SumEncoding (..))
import qualified Data.Aeson.TH as A
import           Data.Aeson.Types (camelTo2)
import           Data.Char
import           Data.List     (findIndex)

-- | Options for aeson TH generator, that generates following fields:
--
-- * without punctuation
-- * without lowercase prefix
--
-- And generates constructor tags without uppercase prefixes with
-- 'stripConstructorPrefix'.
--
-- Sums are encoded as one object with only one field corresponding the
-- constructor used.
--
defaultOptions :: Options
defaultOptions =
    A.defaultOptions
    { fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
    , constructorTagModifier = stripConstructorPrefix
    , sumEncoding = ObjectWithSingleField
    , allNullaryToStringTag = True
    }

-- | Same as 'defaultOptions', but don't generate separate JSON object for newtypes
defaultNewtypeOptions :: Options
defaultNewtypeOptions = defaultOptions { unwrapUnaryRecords = True }

-- | These options don't modify constructor tags
--
-- Same as 'defaultOptions' but without prefix dropping for constructors.
--
-- Used for uppercased constructors (e. g. BTC, USD).
leaveTagOptions :: Options
leaveTagOptions = defaultOptions { constructorTagModifier = id }

-- | Options for aeson TH deriver that drops exact prefix and preserves camel
-- case (e.g. "mySuperPower" to "superPower").
--
exactPrefixOptions :: String -> Options
exactPrefixOptions prefix = defaultOptions {
      fieldLabelModifier = headToLower . stripExactPrefix prefix
    , constructorTagModifier = headToLower . stripExactPrefix prefix
    , sumEncoding = ObjectWithSingleField
    }

-- | Options for aeson TH deriver that drops exact prefix and maps camel case
-- to snaky case (e.g. "mySuperPower" to "super_power").
--
exactPrefixOptionsSnaky :: String -> Options
exactPrefixOptionsSnaky prefix = defaultOptions {
      fieldLabelModifier = camelTo2 '_' . stripExactPrefix prefix
    , constructorTagModifier = camelTo2 '_' . stripExactPrefix prefix
    , sumEncoding = ObjectWithSingleField
    }

-- | Converts first symbol to lower case
headToLower :: String -> String
headToLower [] = []
headToLower (x:xs) = toLower x : xs

-- | Drop prefix of name until first upper letter is occured
stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

-- | Strip prefix of name that exactly matches specified prefix
stripExactPrefix
  :: String -- ^ Prefix
  -> String -- ^ Name
  -> String -- ^ Name without prefix
stripExactPrefix = go
  where
    go [] name = name
    go (p : ps) name@(x : xs)
        | p == x = go ps xs
        | otherwise = name
    go _ [] = []

-- | Remove from names things like ' and etc
dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

-- | Drop upper case prefixes from constructor names
--
-- Example:
-- >>> stripConstructorPrefix "ABCombo"
-- "Combo"
--
-- >>> stripConstructorPrefix "Combo"
-- "Combo"
stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
    maybe t (flip drop t . decrementSafe) $ findIndex isLower t
  where
    decrementSafe 0 = 0
    decrementSafe i = i - 1
