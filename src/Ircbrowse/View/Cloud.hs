{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Render a word cloud.

module Ircbrowse.View.Cloud where

import Ircbrowse.View

import Control.Arrow
import Data.Aeson
import Data.Aeson.Text
import Data.Text.Lazy.Builder
import Data.Text (Text)

cloud :: String -> (Int,Int) -> Int -> Int -> [(Text,Integer)] -> Html
cloud elid (width,height) limit scale stats = do
  div !# toValue elid $ mempty
  script $ preEscapedToHtml $ "drawCloud(" <> toLazyText (encodeToTextBuilder (toJSON spec)) <> ")"

  where spec = object ["width"  .= width
                      ,"height" .= height
                      ,"parent" .= ("#" ++ elid)
                      ,"words"  .= take limit words
                      ]
        words = map makeWord lstats
        lstats = map (second (\x -> round (log (fi x)) * fi scale)) stats
        makeWord :: (Text,Integer) -> Value
        makeWord (text,size) = object ["text" .= text,"size" .= size]

cloudScripts :: Html
cloudScripts =
  forM_ ["d3","d3.layout.cloud","drawcloud"] $ \name ->
    script ! src ("/js/" <> name <> ".js") $ mempty
