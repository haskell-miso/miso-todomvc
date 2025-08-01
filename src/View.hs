----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module View (viewModel) where
----------------------------------------------------------------------------
import           Data.Bool
----------------------------------------------------------------------------
import           Miso
import           Miso.String (MisoString)
import qualified Miso.String as S
import qualified Miso.Style as CSS 
----------------------------------------------------------------------------
import           Model
import           Update
----------------------------------------------------------------------------
default (MisoString)
----------------------------------------------------------------------------
viewModel :: Model -> View Msg
viewModel m@Model{..} =
    div_
        [ class_ "todomvc-wrapper"
        ]
        [ section_
            [class_ "todoapp"]
            [ viewInput m _mField
            , viewEntries _mVisibility _mEntries
            , viewControls m _mVisibility _mEntries
            ]
        , infoFooter
        ]
----------------------------------------------------------------------------
viewEntries :: MisoString -> [Entry] -> View Msg
viewEntries visibility' entries =
    section_
        [ class_ "main"
        , CSS.style_ [ "visibility" =: cssVisibility ]
        ]
        [ input_
            [ class_ "toggle-all"
            , type_ "checkbox"
            , name_ "toggle"
            , id_ "toggle-all"
            , checked_ allCompleted
            , onClick $ CheckAll (not allCompleted)
            ]
        , label_
            [for_ "toggle-all"]
            [text $ S.pack "Mark all as complete"]
        , ul_ [class_ "todo-list"] $
            map viewKeyedEntry (filter isVisible entries) 
        ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all _eCompleted entries
    isVisible Entry{..} =
        case visibility' of
            "Completed" -> _eCompleted
            "Active" -> not _eCompleted
            _ -> True
----------------------------------------------------------------------------
viewKeyedEntry :: Entry -> View Msg
viewKeyedEntry = viewEntry
----------------------------------------------------------------------------
viewEntry :: Entry -> View Msg
viewEntry Entry{..} =
    li_
        [ class_ $
            S.intercalate " " $
                ["completed" | _eCompleted] <> ["editing" | _eEditing]
        , key_ _eEid
        ]
        [ div_
            [class_ "view"]
            [ input_
                [ class_ "toggle"
                , type_ "checkbox"
                , checked_ _eCompleted
                , onClick $ Check _eEid (not _eCompleted)
                ]
            , label_
                [onDoubleClick $ EditingEntry _eEid True]
                [text _eDescription]
            , button_
                [ class_ "destroy"
                , onClick $ Delete _eEid
                ]
                []
            ]
        , input_
            [ class_ "edit"
            , value_ _eDescription
            , name_ "title"
            , id_ $ "todo-" <> S.ms _eEid
            , onInput $ UpdateEntry _eEid
            , onBlur $ EditingEntry _eEid False
            , onEnter $ EditingEntry _eEid False
            ]
        ]
----------------------------------------------------------------------------
viewControls :: Model -> MisoString -> [Entry] -> View Msg
viewControls model visibility' entries =
    footer_
        [ class_ "footer"
        , hidden_ (null entries)
        ]
        [ viewControlsCount entriesLeft
        , viewControlsFilters visibility'
        , viewControlsClear model entriesCompleted
        ]
  where
    entriesCompleted = length . filter _eCompleted $ entries
    entriesLeft = length entries - entriesCompleted
----------------------------------------------------------------------------
viewControlsCount :: Int -> View Msg
viewControlsCount entriesLeft =
    span_
        [class_ "todo-count"]
        [ strong_ [] [text $ S.ms entriesLeft]
        , text (item_ <> " left")
        ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)
----------------------------------------------------------------------------
viewControlsFilters :: MisoString -> View Msg
viewControlsFilters visibility' =
    ul_
        [class_ "filters"]
        [ visibilitySwap "#/" "All" visibility'
        , text " "
        , visibilitySwap "#/active" "Active" visibility'
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility'
        ]
----------------------------------------------------------------------------
visibilitySwap :: MisoString -> MisoString -> MisoString -> View Msg
visibilitySwap uri visibility' actualVisibility =
    li_
        []
        [ a_
            [ href_ uri
            , class_ $ S.concat ["selected" | visibility' == actualVisibility]
            , onClick (ChangeVisibility visibility')
            ]
            [text visibility']
        ]
----------------------------------------------------------------------------
viewControlsClear :: Model -> Int -> View Msg
viewControlsClear _ entriesCompleted =
    button_
        [ class_ "clear-completed"
        , prop "hidden" (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [text $ "Clear completed (" <> S.ms entriesCompleted <> ")"]
----------------------------------------------------------------------------
viewInput :: Model -> MisoString -> View Msg
viewInput _ task =
    header_
        [class_ "header"]
        [ h1_ [] [text "todos"]
        , input_
            [ class_ "new-todo"
            , id_ "input-box"
            , placeholder_ "What needs to be done?"
            , autofocus_ True
            , value_ task
            , name_ "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
        ]
----------------------------------------------------------------------------
onEnter :: Msg -> Attribute Msg
onEnter action = onKeyDown $ bool NoOp action . (== KeyCode 13)
----------------------------------------------------------------------------
infoFooter :: View Msg
infoFooter =
    footer_
        [class_ "info"]
        [ p_ [] [text "Double-click to edit a todo"]
        , p_
            []
            [ text "Written by "
            , a_ [href_ "https://github.com/dmjio"] [text "@dmjio"]
            ]
        , p_
            []
            [ text "Part of "
            , a_ [href_ "http://todomvc.com"] [text "TodoMVC"]
            ]
        ]
----------------------------------------------------------------------------

