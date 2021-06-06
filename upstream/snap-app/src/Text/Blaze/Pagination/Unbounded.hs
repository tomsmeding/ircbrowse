{-# LANGUAGE OverloadedStrings #-}

-- | Simple unbounded pagination support for blaze.
module Text.Blaze.Pagination.Unbounded
  (unboundedPagination,UPN(..))
  where

import           Data.Foldable hiding (foldr)
import           Data.Monoid.Operator
import           Network.URI
import           Prelude                     hiding ((++),div,span)
import           Text.Blaze.Extra
import           Text.Blaze.Html5            as H hiding (map)

data UPN a = UPN
    { upnURI :: URI
    , upnResultsPerPage :: Maybe [Integer]  -- ^ options for num per page
    , upnPerPage :: Integer  -- ^ current num per page
    , upnName :: String  -- ^ uri parameter name prefix
    , upnRange :: (a, a)  -- ^ current id range
    , upnHaveMoreLeft :: Bool
    , upnHaveMoreRight :: Bool
    }
  deriving (Show)

-- | Render unbounded pagination as html.
-- The pagination buttons will set either the parameter upnName ++ "_before" or
-- upnName ++ "_after" to an id; both are exclusive ranges.
-- The uri parameter upnName ++ "_per_page" will be set if that preference is
-- changed.
unboundedPagination :: Show a => UPN a -> Html
unboundedPagination upn@UPN{upnURI=uri,upnName=namePrefix} =
  div !. "pagination" $ do
    forM_ (upnResultsPerPage upn) resultsPerPage
    chooser

  where resultsPerPage perPage = do
          div !. "results-per-page" $ do
            "Page size: "
            forM_ perPage $ \count ->
              span !. "per-page-choice" $ do
                let theclass = if count == upnPerPage upn then "current" else ""
                if count == upnPerPage upn
                   then a !. theclass $ toHtml (show count)
                   else a !. theclass ! hrefSet uri (mkparam "per_page") (show count) $
                          toHtml (show count)

        chooser = do
          div !. "pages" $ do
            ul !. "pages-list" $ do
              if upnHaveMoreLeft upn
                then li $ a ! hrefSet uri paramNameBefore (show (fst (upnRange upn))) $ "<<"
                else li !. "disabled" $ a "<<"
              li !. "disabled" $ a "•"
              if upnHaveMoreRight upn
                then li $ a ! hrefSet uri paramNameAfter (show (snd (upnRange upn))) $ ">>"
                else li !. "disabled" $ a ">>"

        paramNameBefore = mkparam "_before"
        paramNameAfter = mkparam "_after"
        mkparam suffix = namePrefix ++ "_" ++ suffix
