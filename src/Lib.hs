{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( main
    ) where

import Prelude hiding (readFile)

import Control.Exception.Base (bracket)
import Control.Monad (unless, void)
import Data.Aeson (eitherDecode, FromJSON)
import Data.ByteString.Lazy (readFile, ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Foldable (for_)
import Data.List (words)
import Database.SQLite.Simple (Only(Only), query, execute, open, close, Connection)
import GHC.Generics
import Network.HTTP.Simple
import System.Process (readProcess)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen), fromAttrib)

newtype Title = Title String

data Cast = Cast
  { title :: Title
  , link :: URL
  }

newtype URL = URL String
  deriving (Generic, Show, FromJSON)

newtype Config = Config
  { targets :: [URL]
  } deriving (Generic, Show, FromJSON)

parseConfig :: ByteString -> Either String Config
parseConfig = eitherDecode

fetchCasts :: URL -> IO [Cast]
fetchCasts (URL url) =
  case parseRequest url of
    Just request -> (matchA =<<) . parseTags . toString .
      getResponseBody <$> httpLBS request
    _ -> putStrLn ("ignoring invalid URL " ++ url) *> pure []
  where
    matchA tag@(TagOpen "a" _) = do
      classNames <- words <$> fromAttrib' "class" tag
      link' <- fromAttrib' "href" tag
      title' <- fromAttrib' "title" tag
      if "yt-uix-tile-link" `elem` classNames
        then pure Cast
          { title = Title title'
          , link = URL ("https://www.youtube.com" ++ link')
          }
        else mempty
    matchA _ =
      mempty
    fromAttrib' attr tag =
      case fromAttrib attr tag of
        "" -> mempty
        x -> pure x

downloadCast :: Connection -> Cast -> IO ()
downloadCast conn Cast {title = Title title', link = URL link'} = do
  exists <-
    not . null <$>
    (query conn "SELECT 1 FROM downloads WHERE link = ?" (Only link') :: IO [Only Int])
  unless exists $ do
    putStrLn $ "downloading " ++ title' ++ " from " ++ link'
    void $ readProcess "youtube-dl"
      [ "-o"
      , "downloads/%(title)s.%(ext)s"
      , "-x"
      , "--audio-format"
      , "mp3"
      , link'
      ] ""
    execute conn "INSERT INTO downloads (link, title, created) VALUES (?, ?, datetime('now'));"
      (link', title')

downloadCasts :: Connection -> URL -> IO ()
downloadCasts conn url = do
  casts <- fetchCasts url
  for_ casts (downloadCast conn)

main :: IO ()
main = do
  config <- parseConfig <$> readFile "config.json"
  case config of
    Right Config {targets} ->
      bracket open' close $
        for_ targets . downloadCasts
    Left errMsg ->
      putStrLn $ "Error parsing config.json: " ++ errMsg
  where
    open' = do
      conn <- open "data"
      execute conn "CREATE TABLE IF NOT EXISTS downloads (link varchar(20) primary key unique, title varchar, created datetime);" ()
      pure conn
