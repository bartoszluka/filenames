{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text as T
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
    , _selectedProjectName :: Maybe Text
    , _stages :: [Text]
    , _selectedStage :: Maybe Text
    , _formats :: [Text]
    , _selectedFormat :: Maybe Text
    , _projectNumbers :: [Int]
    , _selectedProjectNumber :: Maybe Int
    , _versions :: [Int]
    , _selectedVersion :: Maybe Int
    , _finalFilename :: Text
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
        vscroll $
            vstack_
                [childSpacing_ 10]
                [ label "miłego dnia :)"
                , button "Wygeneruj nazwę" GenerateFilename
                , myDropdown selectedClient _clients
                , myDropdown selectedProjectName _projectNames
                , myDropdown selectedStage _stages
                , myDropdown selectedFormat _formats
                , myDropdown selectedProjectNumber _projectNumbers
                , myDropdown selectedVersion _versions
                , textField finalFilename
                ]
                `styleBasic` [padding 10]
    myDropdown lens selector =
        textDropdown lens (Just <$> selector model)

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
    GenerateFilename ->
        let name :: Text
            name =
                T.unwords $
                    (model &)
                        <$> [ showt . toGregorian . _date
                            , showt . _selectedClient
                            ]
         in [Model $ model & finalFilename .~ name]

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
            , _selectedProjectName = Nothing
            , _stages = ["Etap 1", "Etap 2", "Etap 3"]
            , _selectedStage = Nothing
            , _formats = ["FC 16x9", "FC 1x1"]
            , _selectedFormat = Nothing
            , _projectNumbers = [1 .. 4]
            , _selectedProjectNumber = Nothing
            , _versions = [1 .. 4]
            , _selectedVersion = Nothing
            , _finalFilename = ""
            }
