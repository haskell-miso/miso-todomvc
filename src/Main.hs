----------------------------------------------------------------------------
{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
----------------------------------------------------------------------------
import           Model
import           Update
import           View
----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
main :: IO ()
main = run (startComponent app)
----------------------------------------------------------------------------
app :: Component Model Msg
app = (component emptyModel updateModel viewModel)
  { events = defaultEvents <> keyboardEvents
  , initialAction = Just FocusOnInput
  , styles =
      [ Href "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.min.css"
      , Href "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.4.3/index.min.css"
      ]
  }
----------------------------------------------------------------------------
