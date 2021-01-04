{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ircbrowse.View.PerfStats (perfStats) where

import qualified Ircbrowse.PerfStats as Perf
import           Ircbrowse.View
import           Ircbrowse.View.Template

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (getMin, getMax)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Encoding.Error as Enc
import Numeric (showFFloat)


perfStats :: Perf.PerfStats -> Html
perfStats perfStats = do
  template "perf-stats" "Performance statistics" mempty $ do
    container $ do
      mainHeading "Performance statistics"
      row $ span12 $ p "This page contains performance statistics about the server."

      statsTable "Route" (Perf.cRouteTimes perfStats)
      statsTable "Function" (Perf.cFuncTimes perfStats)
    footer

statsTable :: String -> Map ByteString (Perf.Stats Double) -> Html
statsTable keyName statsMap = do
  h3 (string (keyName ++ " timings"))
  table !. "table" $ do
    thead $ tr $ do
      th (string keyName)
      th "Samples"
      th "Mean time"
    tbody $ do
      forM_ (Map.assocs statsMap) $ \(key, stats) -> do
        tr $ do
          td $ toHtml (Enc.decodeUtf8With Enc.lenientDecode key)
          td $ toHtml (getSum (Perf.sCount stats))
          td $ do
            let tMean = getSum (Perf.sTotal stats) / fromIntegral (getSum (Perf.sCount stats))
                tMin = getMin (Perf.sMin stats)
                tMax = getMax (Perf.sMax stats)
            formatTiming (Just 0.5) tMean
            span ! style "color: #888; margin-left: 10px" $ do
              "("
              formatTiming Nothing tMin
              " .. "
              formatTiming (Just 0.5) tMax
              ")"

formatTiming :: Maybe Double -> Double -> Html
formatTiming mthreshold time =
    let base = toHtml $ T.pack (showFFloat (Just 3) (time * 1000) " ms")
    in case mthreshold of
         Just th | time >= th -> b base
         _ -> span base
