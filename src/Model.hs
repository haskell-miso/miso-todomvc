-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Model
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Model where
----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.Lens.TH
import           Miso.String (MisoString, ToMisoString(..))
----------------------------------------------------------------------------
data Entry
  = Entry
  { _eDescription :: MisoString
  , _eCompleted :: Bool
  , _eEditing :: Bool
  , _eEid :: Int
  , _eFocussed :: Bool
  } deriving (Eq)
----------------------------------------------------------------------------
makeLenses ''Entry
----------------------------------------------------------------------------
newEntry :: MisoString -> Int -> Entry
newEntry desc eid
  = Entry
  { _eDescription = desc
  , _eCompleted = False
  , _eEditing = False
  , _eEid = eid
  , _eFocussed = False
  }
----------------------------------------------------------------------------
data Visibility
  = All
  | Active
  | Completed
  deriving (Eq)
----------------------------------------------------------------------------
instance ToMisoString Visibility where
  toMisoString = \case
    All -> "All"
    Active -> "Active"
    Completed -> "Completed"
----------------------------------------------------------------------------
data Model
  = Model
  { _mEntries :: [Entry]
  , _mField :: MisoString
  , _mUid :: Int
  , _mVisibility :: Visibility
  , _mStep :: Bool
  } deriving (Eq)
----------------------------------------------------------------------------
makeLenses ''Model
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel
  = Model
  { _mEntries = []
  , _mVisibility = All
  , _mField = mempty
  , _mUid = 0
  , _mStep = False
  }
----------------------------------------------------------------------------
