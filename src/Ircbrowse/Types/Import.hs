{-# LANGUAGE LambdaCase #-}

module Ircbrowse.Types.Import (
    Channel(Haskell),
    prettyChan, showChan, showChanInt, parseChan, idxNum
) where

import Data.List (find)
import qualified Data.Vector as V

-- | Possible supported channels.
data Channel
  = Haskell
--  | Lisp
--  | HaskellGame
--  | Diagrams
--  | Tasty
--  | HaskellDistributed

--  | FSharp
--  | Ghcjs
--  | HaskellBeginners
  | HLedger
--  | Typelevel
--  | Scalaz
--  | Shapeless
--  | Purescript
--  | HaskellCN
--  | ReflexFrp
--  | HaskellIdeEngine
--  | HaskellStack
--  | Snowdrift
--  | Ghc
--  | Hackage
--  | Servant
--  | CakeML
--  | LibReviews
--  | ProjectM36
  deriving (Eq, Enum, Bounded)

data ChanInfo =
    ChanInfo { ciPretty :: String
             , ciShow :: String
             , ciInt :: Int
             , ciIdxNum :: Int }

infoList :: [(Channel, ChanInfo)]
infoList =
    [(Haskell, ChanInfo "#haskell" "haskell" 1 1000)
    -- ,(Lisp, ChanInfo "#lisp" "lisp" 2 2000)
    -- ,(HaskellGame, ChanInfo "#haskell-game" "haskell-game" 3 3000)
    -- ,(Diagrams, ChanInfo "#diagrams" "diagrams" 4 4000)
    -- ,(Tasty, ChanInfo "#tasty" "tasty" 5 5000)
    -- ,(HaskellDistributed, ChanInfo "#haskell-distributed" "haskell-distributed" 6 6000)

    -- ,(FSharp, ChanInfo "##fsharp" "fsharp" 8 8000)
    -- ,(Ghcjs, ChanInfo "#ghcjs" "ghcjs" 9 9000)
    -- ,(HaskellBeginners, ChanInfo "#haskell-beginners" "haskell-beginners" 10 10000)
    ,(HLedger, ChanInfo "#hledger" "hledger" 11 11000)
    -- ,(Typelevel, ChanInfo "#typelevel" "typelevel" 12 12000)
    -- ,(Scalaz, ChanInfo "#scalaz" "scalaz" 13 13000)
    -- ,(Shapeless, ChanInfo "#shapeless" "shapeless" 14 14000)
    -- ,(Purescript, ChanInfo "#purescript" "purescript" 15 15000)
    -- ,(HaskellCN, ChanInfo "#haskell-cn" "haskell-cn" 16 16000)
    -- ,(ReflexFrp, ChanInfo "#reflex-frp" "reflex-frp" 17 17000)
    -- ,(HaskellIdeEngine, ChanInfo "#haskell-ide-engine" "haskell-ide-engine" 18 18000)
    -- ,(HaskellStack, ChanInfo "#haskell-stack" "haskell-stack" 19 19000)
    -- ,(Snowdrift, ChanInfo "#snowdrift" "snowdrift" 20 20000)
    -- ,(Ghc, ChanInfo "#ghc" "ghc" 21 21000)
    -- ,(Hackage, ChanInfo "#hackage" "hackage" 22 22000)
    -- ,(Servant, ChanInfo "#servant" "servant" 23 23000)
    -- ,(CakeML, ChanInfo "#cakeml" "cakeml" 24 24000)
    -- ,(LibReviews, ChanInfo "#lib.reviews" "lib.reviews" 25 25000)
    -- ,(ProjectM36, ChanInfo "#project-m36" "project-m36" 26 26000)
    ]

infoTable :: V.Vector ChanInfo
infoTable =
    let firstChannel = minBound :: Channel
        lastChannel  = maxBound :: Channel
        numChannels = fromEnum lastChannel - fromEnum firstChannel + 1
    in if all id [length infoList == numChannels
                 ,map fst infoList == [firstChannel..lastChannel]]  -- also requires sorted
           then V.fromListN numChannels (map snd infoList)
           else error "Invalid infoList in Import.hs"

lookupInfo :: Channel -> ChanInfo
lookupInfo ch = infoTable V.! fromEnum ch

-- | Pretty print a channel in a human-representation.
prettyChan :: Channel -> String
prettyChan = ciPretty . lookupInfo

-- | Show a channel.
showChan :: Channel -> String
showChan = ciShow . lookupInfo

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt = ciInt . lookupInfo

idxNum :: Channel -> Int
idxNum = ciIdxNum . lookupInfo

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan name = fst <$> find ((== name) . ciShow . snd) infoList
