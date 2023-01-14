{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Monomer
import Monomer.Lens qualified as L
import TextShow

dupa :: a
dupa = undefined

data AppModel = AppModel
    { _date :: Day
    , _clients :: [Text]
    , _selectedClient :: Maybe Text
    , _projectNames :: [Text]
    , _stages :: [Text]
    , _formats :: [Text]
    , _projectNumbers :: [Int]
    , _versions :: [Int]
    , _finalFilename :: Maybe Text
    }
    deriving (Eq, Show)

data AppEvent
    = Init
    | GetDate
    | AssignDate Day
    | GenerateFilename
    deriving (Eq, Show)

makeLenses 'AppModel

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
        vstack_
            [childSpacing_ 10]
            [ label "miłego dnia :)"
            , button "Wygeneruj nazwę" GenerateFilename
            , -- , textDropdown (clients ) [_clients model]
              textDropdown selectedClient (Just <$> _clients model)
            ]
            `styleBasic` [padding 10]

handleEvent ::
    WidgetEnv AppModel AppEvent ->
    WidgetNode AppModel AppEvent ->
    AppModel ->
    AppEvent ->
    [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
    Init -> []
    GetDate -> [Task $ AssignDate . utctDay <$> getCurrentTime]
    AssignDate day -> [Model $ model & date .~ day]
    GenerateFilename -> [Model $ model & finalFilename ?~ "hey"]

main :: IO ()
main = do
    currentDay <- utctDay <$> getCurrentTime
    startApp (model currentDay) handleEvent buildUI config
  where
    config =
        [ appWindowTitle "filenames"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appInitEvent Init
        ]
    model day =
        AppModel
            { _date = day
            , _clients = ["McD", "Klient 1", "Klient 2"]
            , _selectedClient = Nothing
            , _projectNames = ["Video Świąteczne", "Projekt 1", "Projekt 2"]
            , _stages = ["Etap 1", "Etap 2", "Etap 3"]
            , _formats = ["FC 16x9", "FC 1x1"]
            , _projectNumbers = [1 .. 4]
            , _versions = [1 .. 4]
            , _finalFilename = Nothing
            }
