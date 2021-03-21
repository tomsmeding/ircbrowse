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
  -- | HLedger
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
  | XMonad
  deriving (Eq, Enum, Bounded)

data ChanInfo =
    ChanInfo { ciPretty :: String
             , ciShow :: String
             , ciInt :: Int }

infoList :: [(Channel, ChanInfo)]
infoList =
    [(Haskell, ChanInfo "#haskell" "haskell" 1)
    -- ,(Lisp, ChanInfo "#lisp" "lisp" 2)
    -- ,(HaskellGame, ChanInfo "#haskell-game" "haskell-game" 3)
    -- ,(Diagrams, ChanInfo "#diagrams" "diagrams" 4)
    -- ,(Tasty, ChanInfo "#tasty" "tasty" 5)
    -- ,(HaskellDistributed, ChanInfo "#haskell-distributed" "haskell-distributed" 6)

    -- ,(FSharp, ChanInfo "##fsharp" "fsharp" 8)
    -- ,(Ghcjs, ChanInfo "#ghcjs" "ghcjs" 9)
    -- ,(HaskellBeginners, ChanInfo "#haskell-beginners" "haskell-beginners" 10)
    -- ,(HLedger, ChanInfo "#hledger" "hledger" 11)
    -- ,(Typelevel, ChanInfo "#typelevel" "typelevel" 12)
    -- ,(Scalaz, ChanInfo "#scalaz" "scalaz" 13)
    -- ,(Shapeless, ChanInfo "#shapeless" "shapeless" 14)
    -- ,(Purescript, ChanInfo "#purescript" "purescript" 15)
    -- ,(HaskellCN, ChanInfo "#haskell-cn" "haskell-cn" 16)
    -- ,(ReflexFrp, ChanInfo "#reflex-frp" "reflex-frp" 17)
    -- ,(HaskellIdeEngine, ChanInfo "#haskell-ide-engine" "haskell-ide-engine" 18)
    -- ,(HaskellStack, ChanInfo "#haskell-stack" "haskell-stack" 19)
    -- ,(Snowdrift, ChanInfo "#snowdrift" "snowdrift" 20)
    -- ,(Ghc, ChanInfo "#ghc" "ghc" 21)
    -- ,(Hackage, ChanInfo "#hackage" "hackage" 22)
    -- ,(Servant, ChanInfo "#servant" "servant" 23)
    -- ,(CakeML, ChanInfo "#cakeml" "cakeml" 24)
    -- ,(LibReviews, ChanInfo "#lib.reviews" "lib.reviews" 25)
    -- ,(ProjectM36, ChanInfo "#project-m36" "project-m36" 26)
    ,(XMonad, ChanInfo "#xmonad" "xmonad" 27)
    ]

infoTable :: V.Vector ChanInfo
infoTable =
    let firstChannel = minBound :: Channel
        lastChannel  = maxBound :: Channel
        numChannels = fromEnum lastChannel - fromEnum firstChannel + 1
    in if and [length infoList == numChannels
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
idxNum = (*1000) . showChanInt

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan name = fst <$> find ((== name) . ciShow . snd) infoList
