----------------------------------------------------------------------------
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
module Update (Msg(..), updateModel) where
----------------------------------------------------------------------------
import           Miso
import           Miso.Lens as Lens
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
  | ChangeVisibility MisoString
  | FocusOnInput
  deriving (Show)
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
  mEntries %= \es -> 
    filterMap es ((== id') . _eEid) (Lens.set eEditing isEditing . Lens.set eFocussed isEditing)
updateModel (UpdateEntry id' task) =
  mEntries %= \es ->
    filterMap es ((== id') . _eEid) (Lens.set eDescription task)
updateModel (Delete id') =
  mEntries %= filter ((/= id') . _eEid)
updateModel DeleteComplete =
  mEntries %= filter (not . _eCompleted)
updateModel (Check id' isCompleted) =
  mEntries %= \es ->
    filterMap es ((== id') . _eEid) (Lens.set eCompleted isCompleted)
updateModel (CheckAll isCompleted) =
  mEntries %= \es ->
    filterMap es (const True) (Lens.set eCompleted isCompleted)
updateModel (ChangeVisibility v) =
    mVisibility .= v
----------------------------------------------------------------------------
-- TODO refact?
filterMap :: [a] -> (a -> Bool) -> (a -> a) -> [a]
filterMap xs predicate f = go' xs
  where
    go' [] = []
    go' (y : ys)
        | predicate y = f y : go' ys
        | otherwise = y : go' ys
----------------------------------------------------------------------------
