{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Prelude hiding (head, putStrLn, readFile, words)
import Control.Monad
import Data.Functor ((<&>))
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Text (unpack, strip, words, Text, isInfixOf)
import Data.Text.IO
import Data.Maybe (listToMaybe)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
import Text.HTML.Scalpel

data Target = Target
  { tagOf, textOf :: Text
  , classOf :: NonEmpty Text
  } deriving Show

type Score = Int
type Rule = Scraper Text Score

data Result = Result
  { contentOf :: Text
  , scoreOf :: Score }

instance Eq Result where
  (==) = (==) `on` scoreOf

instance Ord Result where
  compare = compare `on` scoreOf

here :: Selector
here = anySelector `atDepth` 0

classAttr :: String
classAttr = "class"

rules :: Target -> [Rule]
rules = \case
  Target { textOf = text', classOf = class' } -> let
    contains = text here <&> contains' >>= guard >> return 2
      where contains' = isInfixOf text'
    sameClass = attr classAttr here <&> sameClass' . words >>= guard >> return 1
      where sameClass' classes = flip elem classes `all` toList class'
    in
      [ contains
      , sameClass ]

pick :: Text -> Target -> Maybe Text
pick document target =
  let selector = tagSelector . unpack $ tagOf target in
  best $ scrapeStringLike document
    $ chroots selector $ do
      tag <- html here
      let score = scrapeStringLike tag <$> rules target
      pure $ Result tag $ sum $ sum <$> score
  where
    best rule = rule >>= nonEmpty <&> contentOf . maximum

from :: Text -> Maybe Target
from document =
  let selector = AnyTag @: [ idAttr @= targetID ] in
  join $ scrapeStringLike document
    $ chroot selector $ do
      tag_ <- html here
      text_ <- text here
      class_ <- attr classAttr here

      let tag' = tagName =<< getTag tag_
          text' = pure $ strip text_
          class' = nonEmpty $ words class_
      return $ liftM3 Target tag' text' class'
    where
      idAttr = "id"
      getTag = listToMaybe . parseTags
      tagName = \case
        TagOpen name _ -> Just name
        _ -> Nothing
      targetID = "make-everything-ok-button"

main :: IO ()
main = do
  originFile <- readFile originPath
  sampleFile <- readFile samplePath
  sequence_ $ print . show <$> do
    target <- from originFile
    pick sampleFile target
  where
      originPath = "data/sample-0-origin.html"
      samplePath = "data/sample-1-evil-gemini.html"
      -- samplePath' = "data/sample-2-container-and-clone.html"
      -- samplePath'' = "data/sample-3-the-escape.html"
      -- samplePath''' = "data/sample-4-the-mash.html"
