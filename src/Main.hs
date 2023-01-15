{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception (try)
import Control.Lens
import Data.Text qualified as T
import Data.Time.Calendar
import Data.Time.Clock
import Monomer
import Relude
import System.IO.Error (IOError)
import Text.ParserCombinators.ReadPrec qualified as R
import Text.Read (Read (..))
import TextShow
import Prelude qualified

readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . T.unpack

newtype Version = Version Int deriving (Eq, Ord)

instance Show Version where
    show (Version v) = "v" <> show v

instance TextShow Version where
    showb = TextShow.fromString . show

instance Read Version where
    readPrec = do
        maybeV <- R.get
        if maybeV == 'v'
            then Version <$> readPrec
            else R.pfail

data AppModel = AppModel
    { _date :: Day
    , _clients :: [Text]
    , _selectedClient :: Text
    , _projectNames :: [Text]
    , _selectedProjectName :: Text
    , _stages :: [Text]
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
    | SaveCompleted ()
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
                [ myDropdown "klient:" selectedClient (^. clients) id
                , myDropdown "projekt:" selectedProjectName (^. projectNames) id
                , myDropdown "etap:" selectedStage (^. stages) id
                , myDropdown "format:" selectedFormat (^. formats) id
                , myDropdown "numer projektu:" selectedProjectNumber (^. projectNumbers) (("p" <>) . (showt :: Int -> Text))
                , myDropdown "wersja:" selectedVersion (^. versions) showt
                , button "wygeneruj nazwę" GenerateFilename
                , textField finalFilename
                ]
                `styleBasic` [padding 10]
    -- myDropdown :: Text -> ALens' AppModel (Maybe a) -> (AppModel -> a)
    myDropdown text field selector customShow =
        hstack [label text, spacer, dropdown field (selector model) selected row]
      where
        selected item = label $ customShow item
        row item = label $ customShow item

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
    GenerateFilename -> [Model $ model & finalFilename .~ name, Task $ saveToFile model]
    SaveCompleted _ -> []
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
    formatDate m =
        let (year, month, day) = toGregorian (_date m)
         in showt (year `mod` 100)
                <> (if month < 10 then "0" <> showt month else showt month)
                <> showt day

saveToFile :: AppModel -> TaskHandler AppEvent
saveToFile model = do
    _ <-
        writeFile ".filenames" . T.unpack . T.unlines $
            let commaSep = T.intercalate ","
             in [ "clients:" <> commaSep (model ^. clients)
                , "selectedClient:" <> model ^. selectedClient
                , "projectNames:" <> commaSep (model ^. projectNames)
                , "selectedProjectName:" <> model ^. selectedProjectName
                , "stages:" <> commaSep (model ^. stages)
                , "selectedStage:" <> model ^. selectedStage
                , "formats:" <> commaSep (model ^. formats)
                , "selectedFormat:" <> model ^. selectedFormat
                , "projectNumbers:" <> (commaSep . map showt $ model ^. projectNumbers)
                , "selectedProjectNumber:" <> showt (model ^. selectedProjectNumber)
                , "versions:" <> (commaSep . map showt $ (model ^. versions))
                , "selectedVersion:" <> showt (model ^. selectedVersion)
                ]
    return (SaveCompleted ())

main :: IO ()
main = do
    currentDay <- utctDay <$> getCurrentTime
    strOrExc <- try $ readFileBS ".filenames"
    let appModel = case strOrExc of
            Left (ex :: IOError) -> defaultModel currentDay
            Right contents ->
                parseConfig currentDay (decodeUtf8With lenientDecode contents)
    startApp appModel handleEvent buildUI config
  where
    config =
        [ appWindowTitle "filenames"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appInitEvent Init
        ]

    defaultModel day =
        AppModel
            { _date = day
            , _clients = []
            , _selectedClient = ""
            , _projectNames = []
            , _selectedProjectName = ""
            , _stages = []
            , _selectedStage = ""
            , _formats = []
            , _selectedFormat = ""
            , _projectNumbers = []
            , _selectedProjectNumber = 0
            , _versions = []
            , _selectedVersion = Version 0
            , _finalFilename = ""
            }

parseConfig :: Day -> Text -> AppModel
parseConfig day content =
    AppModel
        { _date = day
        , _clients = clients_
        , _selectedClient = selectedClient_
        , _projectNames = projectNames_
        , _selectedProjectName = selectedProjectName_
        , _stages = stages_
        , _selectedStage = selectedStage_
        , _formats = formats_
        , _selectedFormat = selectedFormat_
        , _projectNumbers = projectNumbers_
        , _selectedProjectNumber = selectedProjectNumber_
        , _versions = versions_
        , _selectedVersion = selectedVersion_
        , _finalFilename = ""
        }
  where
    clients_ = fromMaybe [] $ searchList "clients"
    selectedClient_ = fromMaybe "" $ search "selectedClient"
    projectNames_ = fromMaybe [] $ searchList "projectNames"
    selectedProjectName_ = fromMaybe "" $ search "selectedProjectName"
    stages_ = fromMaybe [] $ searchList "stages"
    selectedStage_ = fromMaybe "" $ search "selectedStage"
    formats_ = fromMaybe [] $ searchList "formats"
    selectedFormat_ = fromMaybe "" $ search "selectedFormat"
    projectNumbers_ = fromMaybe [] $ searchList "projectNumbers" >>= traverse readMaybeT
    selectedProjectNumber_ = fromMaybe 0 $ search "selectedProjectNumber" >>= readMaybeT
    versions_ = fromMaybe [] $ searchList "versions" >>= traverse readMaybeT
    selectedVersion_ = fromMaybe (Version 0) $ search "selectedVersion" >>= readMaybeT
    pseudoDict :: [Maybe (Text, Text)]
    pseudoDict = splitKeys <$> T.lines content
    splitKeys input = case T.splitOn ":" input of
        [key, values] -> Just (key, values)
        _ -> Nothing
    searchList :: Text -> Maybe [Text]
    searchList k = T.splitOn "," <$> search k
    search k = do
        mb <-
            single
                . filter
                    ( \case
                        Nothing -> False
                        Just (key, _) -> key == k
                    )
                $ pseudoDict
        (_, v) <- mb
        return v
    single = \case
        [x] -> Just x
        _ -> Nothing
