----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Update
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Update (Msg(..), updateModel) where
----------------------------------------------------------------------------
import           Miso hiding (set)
import           Miso.Lens
import           Miso.String (MisoString)
import qualified Miso.String as S
----------------------------------------------------------------------------
import           Model
----------------------------------------------------------------------------
default (MisoString)
----------------------------------------------------------------------------
data Msg
  = NoOp
  | CurrentTime Int
  | UpdateField MisoString
  | EditingEntry Int Bool
  | UpdateEntry Int MisoString
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility Visibility
  | FocusOnInput
----------------------------------------------------------------------------
updateModel :: Msg -> Effect Model Msg
updateModel NoOp = pure ()
updateModel FocusOnInput =
  io_ (focus "input-box")
updateModel (CurrentTime time) =
  io_ $ consoleLog $ S.ms (show time)
updateModel Add = do
  uid <- use mUid
  field <- use mField
  mUid += 1
  mField .= mempty
  mEntries %= (<> [newEntry field uid | not $ S.null field])
updateModel (UpdateField str) = 
  mField .= str
updateModel (EditingEntry id' isEditing) =
  mEntries %= mapIf ((== id') . _eEid) (set eEditing isEditing . set eFocussed isEditing)
updateModel (UpdateEntry id' task) =
  mEntries %= mapIf ((== id') . _eEid) (set eDescription task)
updateModel (Delete id') =
  mEntries %= filter ((/= id') . _eEid)
updateModel DeleteComplete =
  mEntries %= filter (not . _eCompleted)
updateModel (Check id' isCompleted) =
  mEntries %= mapIf ((== id') . _eEid) (set eCompleted isCompleted)
updateModel (CheckAll isCompleted) =
  mEntries %= map (set eCompleted isCompleted)
updateModel (ChangeVisibility v) =
  mVisibility .= v
----------------------------------------------------------------------------
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map (\x -> if p x then f x else x)
----------------------------------------------------------------------------
