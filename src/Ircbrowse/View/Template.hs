{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ircbrowse.View.Template where

import Ircbrowse.View
import Ircbrowse.Types.Import
import qualified Text.Blaze.Html5 as H
import Data.Text (Text)

template :: AttributeValue -> Text -> Html -> Html -> Html
template name thetitle innerhead innerbody = do
  docType
  html $ do
    head $ do H.title $ toHtml thetitle
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap-responsive.css"
              link ! rel "stylesheet" ! type_ "text/css" ! href "/css/ircbrowse.css"
              meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
              innerhead
    body !# name $ innerbody

channelNav :: Channel -> Html
channelNav channel =
  div !. "navbar navbar-static-top navbar-inverse" $
    div !. "navbar-inner" $ do
      div !. "container" $ do
        a !. "brand" ! href "/" $ "IRCBrowse"
        ul !. "nav" $ do
          li $ a ! href (toValue ("/" ++ showChan channel)) $ do
             (toHtml (prettyChan channel))
          li $ a ! href (toValue ("/browse/" ++ showChan channel)) $ do
             "Browse"
          li $ a ! href (toValue ("/day/" ++ showChan channel ++ "/today/recent")) $ do
             "Recent"
          li $ a ! href (toValue ("/day/" ++ showChan channel ++ "/today")) $ do
             "Today"
          li $ a ! href (toValue ("/calendar/" ++ showChan channel)) $ do
             "Calendar"
          li $ a ! href (toValue ("/nicks/" ++ showChan channel)) $ do
             "Nicks"
          li $ a ! href (toValue ("/pdfs/" ++ showChan channel)) $ do
             "PDFs"

showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest

footer :: Html
footer =
  div !# "footer" $
    div !. "container" $ do
      p !. "muted credit" $ do
        a ! href "http://ircbrowse.tomsmeding.com" $ "IRC Browse"
        " by "
        a ! href "https://chrisdone.com" $ "Chris Done"
        ", run by "
        a ! href "https://tomsmeding.com" $ "Tom Smeding"
        " | "
        a ! href "https://github.com/tomsmeding/ircbrowse" $ "Source code"
        " | "
        a ! href "http://haskell.org/" $ "Haskell"

mainHeading :: Html -> Html
mainHeading inner = h1 $ do
  a ! href "/" $ do "IRC Browse"
  ": "
  inner
