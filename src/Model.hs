----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Model where
----------------------------------------------------------------------------
import           Data.Aeson -- hiding ((.=), Object)
import           GHC.Generics
----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.Lens.TH
import           Miso.String (MisoString)
----------------------------------------------------------------------------
default (MisoString)
----------------------------------------------------------------------------
data Entry
  = Entry
  { _eDescription :: MisoString
  , _eCompleted :: Bool
  , _eEditing :: Bool
  , _eEid :: Int
  , _eFocussed :: Bool
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
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
data Model
  = Model
  { _mEntries :: [Entry]
  , _mField :: MisoString
  , _mUid :: Int
  , _mVisibility :: MisoString
  , _mStep :: Bool
  } deriving stock (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
----------------------------------------------------------------------------
makeLenses ''Model
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel
  = Model
  { _mEntries = []
  , _mVisibility = "All"
  , _mField = mempty
  , _mUid = 0
  , _mStep = False
  }
----------------------------------------------------------------------------
