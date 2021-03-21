{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Ircbrowse.Model.DB (
    EventFilter,
    filterTime, filterChannel, filterTypes,
    countEvents, countDistinctNicks,
) where

import Ircbrowse.Types
import Ircbrowse.Types.Import

import Data.List (intercalate, intersect)
import Database.PostgreSQL.Simple (ToRow, FromRow, (:.)((:.)))
import Snap.App


data EventFilter = EventFilter
  { fTime :: Maybe Range  -- inclusive-exclusive day range
  , fChannel :: Maybe Channel
  , fTypes :: Maybe [String]
  , fInadmissible :: Bool
  }

instance Semigroup EventFilter where
  f <> g =
    let res = EventFilter
          { fTime = combineMaybe rangeIntersect (fTime f) (fTime g)
          , fChannel = combineMaybe' (\_ _ -> Nothing) (fChannel f) (fChannel g)
          , fTypes = combineMaybe intersect (fTypes f) (fTypes g)
          , fInadmissible = bothJust (fChannel f) (fChannel g)
          }
    in res { fInadmissible =
                or [bothJust (fChannel f) (fChannel g)
                   ,maybe False null (fTypes res)]}

instance Monoid EventFilter where
  mempty = EventFilter
    { fTime = Nothing
    , fChannel = Nothing
    , fTypes = Nothing
    , fInadmissible = False
    }

filterTime :: Range -> EventFilter
filterTime r = mempty { fTime = Just r }

filterChannel :: Channel -> EventFilter
filterChannel c = mempty { fChannel = Just c }

filterTypes :: [String] -> EventFilter
filterTypes ts = mempty { fTypes = Just ts }

rangeIntersect :: Range -> Range -> Range
rangeIntersect (Range a b) (Range c d) =
  Range (max a c) (min b d)

countEvents :: EventFilter -> Model c s Integer
countEvents f = do
  result <-
    formatEventFilter f query
        "SELECT COUNT(*) FROM event WHERE"
        ""
  case result of
    [] -> return 0
    Only c : _ -> return c

countDistinctNicks :: EventFilter -> Model c s Integer
countDistinctNicks f = do
  result <-
    formatEventFilter f query
        "SELECT COUNT(DISTINCT nick) FROM event WHERE"
        ""
  case result of
    [] -> return 0
    Only c : _ -> return c

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe f = combineMaybe' ((Just .) . f)

combineMaybe' :: (a -> a -> Maybe a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe' _ Nothing m = m
combineMaybe' _ m Nothing = m
combineMaybe' f (Just a) (Just b) = f a b

bothJust :: Maybe a -> Maybe a -> Bool
bothJust (Just _) (Just _) = True
bothJust _ _ = False

data SomeToRow = forall a. ToRow a => SomeToRow a

data WhereConds = WhereConds [String] [SomeToRow]

instance Semigroup SomeToRow where
  SomeToRow a <> SomeToRow b = SomeToRow (a :. b)

instance Semigroup WhereConds where
  WhereConds cs a <> WhereConds cs' b = WhereConds (cs ++ cs') (a ++ b)

instance Monoid WhereConds where
  mempty = WhereConds [] []

formatEventFilter :: FromRow r
                  => EventFilter
                  -> (forall ps r. (ToRow ps, FromRow r)
                                => [String] -> ps -> Model c s [r])
                  -> String -> String -> Model c s [r]
formatEventFilter filt runner prefix postfix =
  let WhereConds conds params = mconcat
        [apply (fTime filt) $ \(Range from to) ->
           WhereConds ["timestamp >= ? AND timestamp < ?"] [SomeToRow (from, to)]
        ,apply (fChannel filt) $ \ch ->
           WhereConds ["channel = ?"] [SomeToRow (Only (showChanInt ch))]
        ,apply (fTypes filt) $ \case
           [typ] -> WhereConds ["type = ?"] [SomeToRow (Only typ)]
           types -> WhereConds ["type in (" ++
                                  intercalate "," (map (const "?") types) ++ ")"]
                               [SomeToRow types]
        ,if fInadmissible filt
             then WhereConds ["FALSE"] []
             else mempty]
      querystring = prefix ++ " " ++ intercalate " AND " conds ++ " " ++ postfix
  in case params of
       [] -> runner [querystring] ()
       (foldr1 (<>) -> SomeToRow params') ->
         runner [prefix ++ " " ++ intercalate " AND " conds ++ " " ++ postfix]
                params'
  where
    apply :: Monoid b => Maybe a -> (a -> b) -> b
    apply m f = maybe mempty f m
