{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar
import Data.Time.Clock
import Monomer
import Monomer.Lens qualified as L
import Text.Printf (printf)
import TextShow

dupa :: a
dupa = undefined

newtype Version = Version Int deriving (Eq, Ord)

instance Show Version where
    show (Version v) = "v" <> show v

instance TextShow Version where
    showb = fromString . show

data AppModel = AppModel
    { _date :: Day
    , _clients :: [Text]
    , _selectedClient :: Text
    , _projectNames :: [Text]
    , _selectedProjectName :: Text
    , -- pomijalny etap
      _stages :: [Text]
    , _selectedStage :: Text
    , _formats :: [Text]
    , _selectedFormat :: Text
    , _projectNumbers :: [Int]
    , _selectedProjectNumber :: Int
    , _versions :: [Version]
    , _selectedVersion :: Version
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
                [ label "Miłego dnia :)"
                , label $ T.pack (show $ _date model)
                , myDropdown "klient:" selectedClient _clients id
                , myDropdown "projekt:" selectedProjectName _projectNames id
                , myDropdown "etap:" selectedStage _stages id
                , myDropdown "format:" selectedFormat _formats id
                , myDropdown "numer projektu:" selectedProjectNumber _projectNumbers (("p" <>) . (showt :: Int -> Text))
                , myDropdown "wersja:" selectedVersion _versions showt
                , button "wygeneruj nazwę" GenerateFilename
                , textField finalFilename
                ]
                `styleBasic` [padding 10]
    -- myDropdown :: Text -> ALens' AppModel (Maybe a) -> (AppModel -> a)
    myDropdown text lens selector toText =
        hstack [label text, spacer, dropdown lens (selector model) selected row]
      where
        selected item = label $ toText item
        row item = label $ toText item

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
    GenerateFilename -> [Model $ model & finalFilename .~ name]
  where
    name :: Text
    name =
        T.unwords $
            (model &)
                <$> [ formatDate
                    , _selectedClient
                    , _selectedProjectName
                    , _selectedStage
                    , _selectedFormat
                    , showt . _selectedProjectNumber
                    , showt . _selectedVersion
                    ]
    formatDate model =
        let (year, month, day) = toGregorian (_date model)
         in showt (year `mod` 100)
                <> (if month < 10 then "0" <> showt month else showt month)
                <> showt day

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
            , _selectedClient = ""
            , _projectNames = ["Video Świąteczne", "Projekt 1", "Projekt 2"]
            , _selectedProjectName = ""
            , _stages = ["Etap 1", "Etap 2", "Etap 3"]
            , _selectedStage = ""
            , _formats = ["FC 16x9", "FC 1x1"]
            , _selectedFormat = ""
            , _projectNumbers = [1 .. 4]
            , _selectedProjectNumber = 0
            , _versions = Version <$> [1 .. 4]
            , _selectedVersion = Version 0
            , _finalFilename = ""
            }
