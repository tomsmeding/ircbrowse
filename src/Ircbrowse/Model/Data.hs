{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Generate data needed for statistics and things like that. General
-- data that doesn't need to be generated on-demand.

module Ircbrowse.Model.Data where

import Ircbrowse.Monads
import Ircbrowse.Types.Import

import Snap.App

-- | Generate everything.
generateData :: Model c s ()
generateData = do
  void $ do
    exec ["delete from conversation_by_year"] ()
    exec ["delete from general_activity_by_year"] ()
    forM_ [toEnum 0..] $ \channel -> do
      io (putStrLn ("Generating data for #" ++ showChan channel ++ " ..."))
      let cid = showChanInt channel

      io (putStrLn ("  Generating stats ..."))
      exec ["insert into conversation_by_year select date_part('year',timestamp),count(*),? from event where channel = ? and type in ('talk','act') group by date_part('year',timestamp) order by 1;"] (cid,cid)
      exec ["insert into general_activity_by_year select date_part('year',timestamp),count(*),? from event where channel = ? group by date_part('year',timestamp) order by 1;"] (cid,cid)

      io (putStrLn ("  Generating PDF indexes ..."))
      exec ["delete from event_order_index where idx = (? * 1000) + 1"]
           (Only (idxNum channel))
      exec ["insert into event_order_index"
           ,"SELECT RANK() OVER(ORDER BY id desc) AS id,"
           ,"id as origin, "
           ,"(channel * 1000) + 1 as idx "
           ,"FROM event "
           ,"WHERE channel = ? and "
           ,"text LIKE '%http%.pdf%' AND "
           ,"text ~ ?"
           ,"order by id asc;"]
           (showChanInt channel,"https?://[^ ]+\\.pdf")
