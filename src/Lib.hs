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
import Data.Foldable (traverse_, for_)
import Data.List (words)
import Database.SQLite.Simple (Only(Only), query, execute, open, close, Connection)
import GHC.Generics
import System.Process (readProcess)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen), fromAttrib)

newtype Title = Title String
  deriving (Generic, Show)

data Cast = Cast
  { title :: Title
  , link :: URL
  } deriving (Generic, Show)

newtype URL = URL String
  deriving (Generic, Show, FromJSON)

data Config = Config
  { targets :: [URL]
  } deriving (Generic, Show, FromJSON)

parseConfig :: ByteString -> Either String Config
parseConfig = eitherDecode

fetchCasts :: URL -> IO [Cast]
fetchCasts (URL url) =
  extractCasts . parseTags <$>
    readProcess "curl" ["-s", "--get", url] ""
  where
    extractCasts =
      (=<<) matchA
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
  traverse_ (downloadCast conn) casts

main :: IO ()
main = do
  config <- parseConfig <$> readFile "config.json"
  case config of
    Right Config {targets} ->
      bracket (open "data") close $
        for_ targets . downloadCasts
    Left errMsg ->
      putStrLn $ "Error parsing config.json: " ++ errMsg
