{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Lektor where

import Protolude hiding ((<.>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Data.Time
import qualified Data.Text as T
import qualified Data.List as L
import System.FilePath ((</>), (<.>), takeDirectory)
import System.Directory (createDirectoryIfMissing,doesFileExist, getCurrentDirectory)

import Event
import AppCfg


class LektorContent a where
  fromLektorContent :: Text -> Either Text (Maybe a)
  toLektorContent :: a -> Text

type Parser = Parsec Void Text

instance LektorContent Event where

  fromLektorContent = first (toS . errorBundlePretty) . parse lektorContent "parse 'Event' from lektor content"
    where lektorContent :: Parser (Maybe Event)
          lektorContent = do
            model <- model
            case model of
              "event" -> Just <$> (Event
                                 <$> title
                                 <*> (date <* tpe)
                                 <*> url
                                 <*> body)
              _ -> pure Nothing

          model = string' "_model:" *> space *> line <* sep
          title = string' "title:" *> space *> line <* sep
          date = (string' "date:" *> space *> line >>= parseTimeM True defaultTimeLocale "%F %R" . toS) <* sep
          tpe = string' "type:" *> space *> line <* sep
          url = string' "url:" *> space *> line <* sep
          body = string' "body:" *> space *> textBlock

          line = takeWhileP Nothing (/= '\n') <* eol
          lines = T.pack <$> manyTill anySingle (string "----")
          sep = string "---" *> eol
          textBlock = string' "#### text-block ####" *> eol *> space *> string' "text:" *> space *> lines



  toLektorContent Event{..} = T.unlines $ T.unwords <$> L.intersperse ["---"]
    [ [ "_model: event" ]
    , [ "title:", eventTitle ]
    , [ "date:", showUTCTime eventDate]
    , [ "type: Event"]
    , [ "url:", eventUrl]
    , [ "body:\n#### text-block ####\n", "text:", eventText]
    , [ "class: default"]
    ]
    where showUTCTime = toS . formatTime defaultTimeLocale "%F %R"



createLektorCalendarEntry :: (MonadIO m, MonadReader AppCfg m) => Event -> m ()
createLektorCalendarEntry event@(Event{..}) = do
  workDir <- asks cfgWorkDir
  basePath <- (</> (workDir </> "content/kalender")) <$> liftIO getCurrentDirectory
  let path = basePath </> formatTime defaultTimeLocale "%Y/%Y-%m-%d/contents.lr" eventDate
      content = toLektorContent event

  putText $ "schedule " <> eventTitle <> " (" <> show eventDate <> ")"
  liftIO $ ifM (doesFileExist path)
    (putText "  event exists")
    (ifM (doesFileExist $ path <.> "ignore")
      (putText "  contents.lr.ignore file exists - don't schedule the event")
      (do
          putText $ "  create event (" <> toS path <> ")"
          createDirectoryIfMissing True $ takeDirectory path
          writeFile path content))



-- |
loadLektorEventTemplate :: (MonadIO m, MonadReader AppCfg m) => Text -> m Event
loadLektorEventTemplate name = do
  workDir <- asks cfgWorkDir
  basePath <- (</> (workDir </> "templates/chaostreff-scheduler")) <$> liftIO getCurrentDirectory
  content <- liftIO $ readFile (basePath </> toS name)
  case fromLektorContent content of
    (Right (Just event)) -> pure event
    (Right Nothing)      -> liftIO $ die "invalid template - file doesn't contain a 'Event' template"
    (Left msg)           -> liftIO $ die msg
