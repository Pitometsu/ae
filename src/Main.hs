{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (head, putStrLn, readFile, words)
import Control.Monad
import Control.Applicative
import Data.Functor ((<&>))
import Data.Foldable (fold)
import Data.List.NonEmpty (head, NonEmpty, nonEmpty, toList)
import Data.Text (append, cons, pack, unpack, strip, words, Text, isInfixOf)
import Data.Text.IO
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup (maybeTagText, parseTags, Tag(TagOpen))
import Text.HTML.Scalpel

data Target = Target
  { tagOf, textOf :: Text
  , classOf :: NonEmpty Text
  } deriving (Show)

here = anySelector `atDepth` 0
classAttr = "class"

pick :: Text -> Target -> Maybe Text
pick document target =
  let selector = tagSelector . unpack $ tagOf target in
  scrapeStringLike document
    $ chroot selector $ do
      (tag_ :: Text) <- html here
      (text_ :: Text) <- text here
      class_ <- attr classAttr here
      let class' = words class_
      guard $ textOf target `isInfixOf` text_
        || all (flip elem class') (toList $ classOf target)
      return tag_

target :: Text -> Maybe Target
target document =
  let selector = AnyTag @: [ "id" @= targetID ] in
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
      getTag = fmap head . nonEmpty . parseTags
      tagName = \case
        TagOpen name _ -> Just name
        _ -> Nothing
      targetID = "make-everything-ok-button"

main :: IO ()
main = do
  originFile <- readFile originPath
  sampleFile <- readFile samplePath
  sequenceA $ print . show <$> do
    target' <- target originFile
    pick sampleFile target'
  return ()
  where
      originPath = "data/sample-0-origin.html"
      samplePath = "data/sample-1-evil-gemini.html"
      samplePath' = "data/sample-2-container-and-clone.html"
      samplePath'' = "data/sample-3-the-escape.html"
      samplePath''' = "data/sample-4-the-mash.html"
