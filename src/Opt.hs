module Opt
  ( opts
  , parseOpts
  ) where

import Paths_kona (version)
import Utils
import Config

import Data.Monoid
import Data.Version (showVersion)

import Options.Applicative

import Network.HTTP.Req hiding (header)

data Opt =
  Opt [String] -- Tags
      String -- Kind
      String -- Rating
      String -- Order
      Int -- Max worker
      FilePath -- Output
      Int -- Delay
      Int -- Retries
      Int -- Limit
      FilePath -- Exclusion file

opt :: Parser Opt
opt =
  Opt <$> some (argument str (metavar "TAGS...")) <*>
  option
    (eitherReader $ checkWithin ratings)
    (long "rating" <> short 'r' <>
     help
       "Hentainess of the images. \
       \Choose from: safe, questionable, explicit, \
       \questionableminus, questionableplus" <>
     showDefault <>
     value "safe" <>
     completeWith ratings <>
     metavar "RATING") <*>
  option
    (eitherReader $ checkWithin kinds)
    (long "kind" <> short 'k' <>
     help "Kind of image to download. \
          \Choose from: preview, sample, origin" <>
     showDefault <>
     value "origin" <>
     completeWith kinds <>
     metavar "KIND") <*>
  option
    (eitherReader $ checkWithin orders)
    (long "order" <> short 'O' <>
     help "Order of the posts. \
          \Choose from: id, id_desc, score, score_asc, mpixels, mpixels_asc" <>
     showDefault <>
     value "score" <>
     completeWith orders <>
     metavar "ORDER") <*>
  option
    auto
    (long "max_worker" <> short 'w' <>
     help "Limit numbers of task in parallel" <>
     showDefault <>
     value 16 <>
     metavar "MAX_WORKER") <*>
  strOption
    (long "output" <> short 'o' <> help "Output path" <> showDefault <>
     value "images" <>
     metavar "OUTPUT") <*>
  option
    auto
    (long "delay" <> short 'd' <> help "Start next try after DELAY ms" <>
     showDefault <>
     value 100 <>
     metavar "DELAY") <*>
  option
    auto
    (long "retries" <>
     help "Retry RETRIES times, otherwise the program will fail" <>
     showDefault <>
     value 5 <>
     metavar "RETRIES") <*>
  option
    auto
    (long "limit" <> short 'l' <>
     help "Numbers of image to download, 0 for all" <>
     showDefault <>
     value 0 <>
     metavar "LIMIT"
    ) <*>
  strOption
    (long "exclude" <> short 'e' <>
     help "A file consists of excluded MD5s, one per line" <>
     value "" <>
     metavar "EXCLUSION_FILE"
    )
  where
    ratings =
      [ "safe"
      , "questionable"
      , "explicit"
      , "questionableminus"
      , "questionableplus"
      ]
    kinds = ["preview", "sample", "origin"]
    orders = ["id", "id_desc", "score", "score_asc", "mpixels", "mpixels_asc"]
    checkWithin sets v
      | v `elem` sets = Right v
      | otherwise = Left $ "Unknown parameter: " ++ v

opts = info (opt <**> helper)
  ( fullDesc
  <> progDesc "Download all images about TAGS"
  <> header ("kona " ++ showVersion version ++ " – A Crawler for Konachan.com"))

parseOpts (Opt tag rtg knd ord workers out delay retries limit exclusion) =
  CrawlerConfig
    (mkParams [tags (rating rtg : order ord : tag)])
    workers
    limit
    exclusion
    (PostConfig knd out (httpConfig delay retries))
