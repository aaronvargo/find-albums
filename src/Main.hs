{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Control.Monad.IO.Class
import Data.Maybe
import Data.String
import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Data.List(nub)
import Control.Lens

import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Types.URI
import Utils.Spoty
import Text.HTML.TagSoup
import Text.StringLike

tailIf :: [t] -> [t]
tailIf [] = []
tailIf (_:xs) = xs

sects
  :: (StringLike str) => String -> [Tag str] -> [[Tag str]]
sects s = sections (~== s)

getFirst :: StringLike str => String -> [Tag str] -> [Tag str]
getFirst s = head . sects s

fstTagText
  :: (StringLike a, StringLike b, Foldable t, Functor t)
  => t (Tag a) -> Maybe b
fstTagText = fmap castString . asum . fmap maybeTagText

torrentDownloadsURL :: IsString t => t
torrentDownloadsURL = "https://www.torrentdownloads.me"

searchURL :: (StringLike a, StringLike b) => a -> b
searchURL s =
  castString
    (torrentDownloadsURL <> "/search/?search=" <> urlEncode True (castString s))

data Torrent = Torrent
  { _title :: String
  , _seeds :: Int
  , _leeches :: Int
  , _link :: String
  } deriving (Show, Eq)

makeLenses ''Torrent

findTorrents :: (StringLike a, Show a) => [Tag a] -> [Torrent]
findTorrents tgs = catMaybes $ fmap parseTorrent hits
  where
    parseTorrent s = do
      seeds' <- readMaybe =<< fstTagText (sects "<span>" s !! 2)
      leeches' <- readMaybe =<< fstTagText (sects "<span>" s !! 1)
      title' <- fstTagText . getFirst "<a>" $ getFirst "<p>" s
      let link' =
            (torrentDownloadsURL ++) .
            toString . fromAttrib "href" . head . getFirst "<a>" $
            getFirst "<p>" s
      return $ Torrent title' seeds' leeches' link'
    hits = tailIf $ tailIf greyBars
      where
        innCnt = sects "<div class=inner_container>" tgs !! 1
        greyBars = sections f innCnt
        f x = (x ~== ("<div class=grey_bar3>" :: String))
              || (x ~== ("<div class=\"grey_bar3 back_none\">" :: String))

doTorrentSearch :: MonadIO f => T.Text -> f [Torrent]
doTorrentSearch s =
  findTorrents . parseTags <$> simpleHttp (searchURL s)

ppTorrent :: Torrent -> IO ()
ppTorrent t =
  mapM_ putStrLn
  [ "title: " ++ _title t
  , "seeds: " ++ show (_seeds t)
  , "leeches: " ++ show (_leeches t)
  , "link: " ++ _link t
  ]

artistAlbums :: Artist -> IO [Album]
artistAlbums artist = fetchAll . getArtistAlbums $ _artistSpotifyID artist

searchArtistAlbums :: T.Text -> IO (Maybe [Album])
searchArtistAlbums artist =
  traverse artistAlbums =<< fetchOne (searchArtist artist)

runArtist :: T.Text -> IO ()
runArtist artist = do
  searchArtistAlbums artist >>= \case
    Nothing -> putStrLn "Could not find artist."
    Just albums -> do
      let albumNames = nub $ fmap _albumName albums
      putStrLn "Albums:"
      mapM_ T.putStrLn albumNames
      putStrLn ""
      mapM_ runAlbum albumNames
  where runAlbum albm = do
          let searchTerm = artist <> " - " <> albm
          -- let searchTerm = albm
          ts <- doTorrentSearch searchTerm
          case ts ^? _head of
            Nothing -> T.putStrLn $ "No torrent found for search: " <> searchTerm
            Just t -> ppTorrent t
          putStrLn ""

main :: IO ()
main = do
  putStr "Enter an artist: "
  s <- T.getLine
  putStrLn ""
  runArtist s
