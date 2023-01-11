{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import Monomer.Lens qualified as L

data AppModel = AppModel
    { _clickCount :: Int
    , _someText :: Text
    }
    deriving (Eq, Show)

data AppEvent
    = AppInit
    | AppIncrease
    deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
        vstack
            [ label "Hello world"
            , spacer
            , hstack
                [ label $ "Click count: " <> showt (model ^. clickCount)
                , spacer
                , button "Increase count" AppIncrease
                , spacer
                , textField someText
                ]
            ]
            `styleBasic` [padding 10]

handleEvent ::
    WidgetEnv AppModel AppEvent ->
    WidgetNode AppModel AppEvent ->
    AppModel ->
    AppEvent ->
    [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
    AppInit -> []
    AppIncrease -> [Model (model & clickCount +~ 1)]

main :: IO ()
main = do
    startApp model handleEvent buildUI config
  where
    config =
        [ appWindowTitle "filenames"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appInitEvent AppInit
        ]
    model = AppModel 0 ""
