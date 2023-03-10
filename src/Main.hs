{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception (try)
import Control.Lens
import Data.Text qualified as T
import Data.Time.Calendar
import Data.Time.Clock
import FilteredDropdown (filteredDropdown)
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
    , _newClient :: Text
    , _includeClient :: Bool
    , _projectNames :: [Text]
    , _selectedProjectName :: Text
    , _newProjectName :: Text
    , _includeProjectName :: Bool
    , _stages :: [Text]
    , _selectedStage :: Text
    , _newStage :: Text
    , _includeStage :: Bool
    , _formats :: [Text]
    , _selectedFormat :: Text
    , _newFormat :: Text
    , _includeFormat :: Bool
    , _projectNumbers :: [Text]
    , _selectedProjectNumber :: Text
    , _newProjectNumber :: Text
    , _includeProjectNumber :: Bool
    , _versions :: [Text]
    , _selectedVersion :: Text
    , _newVersion :: Text
    , _includeVersion :: Bool
    , _finalFilename :: Text
    }
    deriving (Eq, Show)

type MyLensInto a = Lens' AppModel a

data AppEvent
    = Init
    | GetDate
    | AssignDate Day
    | GenerateFilename
    | SaveCompleted ()
    | Quit
    | AddNew
        (MyLensInto Text)
        -- ^ stores new value
        (MyLensInto Text)
        -- ^ selected field in the list
        (MyLensInto [Text])
        -- ^ list to add the new field to

makeLenses 'AppModel

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent

type MyWidgetNode = WidgetNode AppModel AppEvent

buildUI wenv model = widgetTree
  where
    widgetTree =
        vscroll $
            vstack_
                [childSpacing_ 10]
                -- `id` because maybe in the future there will be different type than `Text`
                [ myWidget "klient:" newClient selectedClient clients includeClient
                , myWidget "projekt:" newProjectName selectedProjectName projectNames includeProjectName
                , myWidget "etap:" newStage selectedStage stages includeStage
                , myWidget "format:" newFormat selectedFormat formats includeFormat
                , myWidget "numer projektu:" newProjectNumber selectedProjectNumber projectNumbers includeProjectNumber
                , myWidget "wersja:" newVersion selectedVersion versions includeVersion
                , button "wygeneruj nazw??" GenerateFilename
                , textField finalFilename
                ]
                `styleBasic` [padding 10]
    myWidget :: Text -> MyLensInto Text -> MyLensInto Text -> MyLensInto [Text] -> MyLensInto Bool -> MyWidgetNode
    myWidget text newFieldLens selectedFieldLens listLens includeFieldLens =
        hstack_
            [childSpacing_ 10]
            [ label text
            , textField newFieldLens
            , button "dodaj" $ AddNew newFieldLens selectedFieldLens listLens
            , dropdown selectedFieldLens list selected row
            , labeledCheckbox_ "ma by?? w nazwie?" includeFieldLens [checkboxSquare]
            , filteredDropdown () ()
            ]
      where
        list :: [Text]
        list = filter (caseInsensitiveMatch (model ^. newFieldLens)) $ model ^. listLens
        caseInsensitiveMatch :: Text -> Text -> Bool
        -- caseInsensitiveMatch text1 text2 = T.toLower text1 `T.isInfixOf` T.toLower text2
        caseInsensitiveMatch = T.isInfixOf `on` T.toLower
        selected = label
        row = label

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
    Quit -> [exitApplication]
    AddNew newFieldLens selectedFieldLens listLens ->
        let value = model ^. newFieldLens
         in [ Model $
                model
                    `with` [ listLens %~ (value :)
                           , selectedFieldLens .~ value
                           , newFieldLens .~ ""
                           ]
            ]
  where
    model_ `with` updates = model_ & foldl' (.) id updates
    name :: Text
    name =
        T.unwords . map fst . filter snd $
            [ (formatedDate, True)
            , (model ^. selectedClient, model ^. includeClient)
            , (model ^. selectedProjectName, model ^. includeProjectName)
            , (model ^. selectedStage, model ^. includeStage)
            , (model ^. selectedFormat, model ^. includeFormat)
            , (model ^. selectedProjectNumber, model ^. includeProjectNumber)
            , (model ^. selectedVersion, model ^. includeVersion)
            ]

    formatedDate =
        let (year, month, day) = toGregorian (model ^. date)
         in showt (year `mod` 100)
                <> ((if month < 10 then "0" else "") <> showt month)
                <> showt day

saveToFile :: AppModel -> TaskHandler AppEvent
saveToFile model = do
    _ <-
        writeFile ".filenames" . T.unpack . T.unlines $
            let commaSep = T.intercalate ","
             in [ "clients:" <> commaSep (model ^. clients)
                , "selectedClient:" <> model ^. selectedClient
                , "includeClient:" <> showt (model ^. includeClient)
                , "projectNames:" <> commaSep (model ^. projectNames)
                , "selectedProjectName:" <> model ^. selectedProjectName
                , "includeProjectName:" <> showt (model ^. includeProjectName)
                , "stages:" <> commaSep (model ^. stages)
                , "selectedStage:" <> model ^. selectedStage
                , "includeStage:" <> showt (model ^. includeStage)
                , "formats:" <> commaSep (model ^. formats)
                , "selectedFormat:" <> model ^. selectedFormat
                , "includeFormat:" <> showt (model ^. includeFormat)
                , "projectNumbers:" <> commaSep (model ^. projectNumbers)
                , "selectedProjectNumber:" <> model ^. selectedProjectNumber
                , "includeProjectNumber:" <> showt (model ^. includeProjectNumber)
                , "versions:" <> commaSep (model ^. versions)
                , "selectedVersion:" <> model ^. selectedVersion
                , "includeVersion:" <> showt (model ^. includeVersion)
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
    args <- getArgs
    let startEvent = if "--quit" `elem` args then Quit else Init
    startApp appModel handleEvent buildUI (config startEvent)
  where
    config event =
        [ appWindowTitle "filenames"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        , appInitEvent event
        ]

    defaultModel day =
        AppModel
            { _date = day
            , _clients = []
            , _selectedClient = ""
            , _newClient = ""
            , _includeClient = True
            , _projectNames = []
            , _selectedProjectName = ""
            , _newProjectName = ""
            , _includeProjectName = True
            , _stages = []
            , _selectedStage = ""
            , _newStage = ""
            , _includeStage = True
            , _formats = []
            , _selectedFormat = ""
            , _newFormat = ""
            , _includeFormat = True
            , _projectNumbers = []
            , _selectedProjectNumber = ""
            , _newProjectNumber = ""
            , _includeProjectNumber = True
            , _versions = []
            , _selectedVersion = ""
            , _newVersion = ""
            , _includeVersion = True
            , _finalFilename = ""
            }

parseConfig :: Day -> Text -> AppModel
parseConfig day content =
    AppModel
        { _date = day
        , _clients = clients_
        , _selectedClient = selectedClient_
        , _newClient = ""
        , _includeClient = includeClient_
        , _projectNames = projectNames_
        , _selectedProjectName = selectedProjectName_
        , _newProjectName = ""
        , _includeProjectName = includeProjectName_
        , _stages = stages_
        , _selectedStage = selectedStage_
        , _newStage = ""
        , _includeStage = includeStage_
        , _formats = formats_
        , _selectedFormat = selectedFormat_
        , _newFormat = ""
        , _includeFormat = includeFormat_
        , _projectNumbers = projectNumbers_
        , _selectedProjectNumber = selectedProjectNumber_
        , _newProjectNumber = ""
        , _includeProjectNumber = includeProjectNumber_
        , _versions = versions_
        , _selectedVersion = selectedVersion_
        , _newVersion = ""
        , _includeVersion = includeVersion_
        , _finalFilename = ""
        }
  where
    clients_ = fromMaybe [] $ searchList "clients"
    selectedClient_ = fromMaybe "" $ search "selectedClient"
    includeClient_ = fromMaybe True $ search "includeClient" >>= readMaybeT
    projectNames_ = fromMaybe [] $ searchList "projectNames"
    selectedProjectName_ = fromMaybe "" $ search "selectedProjectName"
    includeProjectName_ = fromMaybe True $ search "includeProjectName" >>= readMaybeT
    stages_ = fromMaybe [] $ searchList "stages"
    selectedStage_ = fromMaybe "" $ search "selectedStage"
    includeStage_ = fromMaybe True $ search "includeStage" >>= readMaybeT
    formats_ = fromMaybe [] $ searchList "formats"
    selectedFormat_ = fromMaybe "" $ search "selectedFormat"
    includeFormat_ = fromMaybe True $ search "includeFormat" >>= readMaybeT
    projectNumbers_ = fromMaybe [] $ searchList "projectNumbers"
    selectedProjectNumber_ = fromMaybe "" $ search "selectedProjectNumber"
    includeProjectNumber_ = fromMaybe True $ search "includeProjectNumber" >>= readMaybeT
    versions_ = fromMaybe [] $ searchList "versions"
    selectedVersion_ = fromMaybe "" $ search "selectedVersion"
    includeVersion_ = fromMaybe True $ search "includeVersion" >>= readMaybeT
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
