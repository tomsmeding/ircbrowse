{-# LANGUAGE LambdaCase #-}

module Ircbrowse.Types.Import (
    Channel(Haskell),
    Network,
    channelsForNetwork,
    showNetwork,
    chanNetwork, prettyChan, prettyChanWithNetwork, showChan, showChanInt, parseChan, idxNum
) where

import Data.List (find)
import qualified Data.Vector as V

data Network = Freenode | Liberachat
  deriving (Eq, Enum, Bounded)

data NetwInfo =
    NetwInfo { niShow :: String }

networkInfoList :: [(Network, NetwInfo)]
networkInfoList =
    [(Freenode, NetwInfo "freenode")
    ,(Liberachat, NetwInfo "liberachat")]

networkInfoTable :: V.Vector NetwInfo
networkInfoTable =
    let firstNetwork = minBound :: Network
        lastNetwork  = maxBound :: Network
        numNetworks = fromEnum lastNetwork - fromEnum firstNetwork + 1
    in if length networkInfoList == numNetworks
               && map fst networkInfoList == [firstNetwork..lastNetwork]  -- also requires sorted
           then V.fromListN numNetworks (map snd networkInfoList)
           else error "Invalid networkInfoList in Import.hs"

lookupNetworkInfo :: Network -> NetwInfo
lookupNetworkInfo netw = networkInfoTable V.! fromEnum netw

showNetwork :: Network -> String
showNetwork = niShow . lookupNetworkInfo

-- | Possible supported channels.
data Channel
-- == Original Freenode channels ==
  = Haskell
--  | Lisp
--  | HaskellGame
--  | Diagrams
--  | Tasty
--  | HaskellDistributed

--  | FSharp
--  | Ghcjs
--  | HaskellBeginners
--  | HLedger
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

-- == Liberachat channels ==
  | LcHaskell
  | LcXMonad
  deriving (Eq, Enum, Bounded)

data ChanInfo =
    ChanInfo { ciNetwork :: Network
             , ciPretty :: String
             , ciShow :: String
             , ciInt :: Int }

infoList :: [(Channel, ChanInfo)]
infoList =
    [(Haskell, ChanInfo Freenode "#haskell" "haskell" 1)
    -- ,(Lisp, ChanInfo Freenode "#lisp" "lisp" 2)
    -- ,(HaskellGame, ChanInfo Freenode "#haskell-game" "haskell-game" 3)
    -- ,(Diagrams, ChanInfo Freenode "#diagrams" "diagrams" 4)
    -- ,(Tasty, ChanInfo Freenode "#tasty" "tasty" 5)
    -- ,(HaskellDistributed, ChanInfo Freenode "#haskell-distributed" "haskell-distributed" 6)

    -- ,(FSharp, ChanInfo Freenode "##fsharp" "fsharp" 8)
    -- ,(Ghcjs, ChanInfo Freenode "#ghcjs" "ghcjs" 9)
    -- ,(HaskellBeginners, ChanInfo Freenode "#haskell-beginners" "haskell-beginners" 10)
    -- ,(HLedger, ChanInfo Freenode "#hledger" "hledger" 11)
    -- ,(Typelevel, ChanInfo Freenode "#typelevel" "typelevel" 12)
    -- ,(Scalaz, ChanInfo Freenode "#scalaz" "scalaz" 13)
    -- ,(Shapeless, ChanInfo Freenode "#shapeless" "shapeless" 14)
    -- ,(Purescript, ChanInfo Freenode "#purescript" "purescript" 15)
    -- ,(HaskellCN, ChanInfo Freenode "#haskell-cn" "haskell-cn" 16)
    -- ,(ReflexFrp, ChanInfo Freenode "#reflex-frp" "reflex-frp" 17)
    -- ,(HaskellIdeEngine, ChanInfo Freenode "#haskell-ide-engine" "haskell-ide-engine" 18)
    -- ,(HaskellStack, ChanInfo Freenode "#haskell-stack" "haskell-stack" 19)
    -- ,(Snowdrift, ChanInfo Freenode "#snowdrift" "snowdrift" 20)
    -- ,(Ghc, ChanInfo Freenode "#ghc" "ghc" 21)
    -- ,(Hackage, ChanInfo Freenode "#hackage" "hackage" 22)
    -- ,(Servant, ChanInfo Freenode "#servant" "servant" 23)
    -- ,(CakeML, ChanInfo Freenode "#cakeml" "cakeml" 24)
    -- ,(LibReviews, ChanInfo Freenode "#lib.reviews" "lib.reviews" 25)
    -- ,(ProjectM36, ChanInfo Freenode "#project-m36" "project-m36" 26)
    ,(XMonad, ChanInfo Freenode "#xmonad" "xmonad" 27)
    ,(LcHaskell, ChanInfo Liberachat "#haskell" "lchaskell" 28)
    ,(LcXMonad, ChanInfo Liberachat "#xmonad" "lcxmonad" 29)
    ]

infoTable :: V.Vector ChanInfo
infoTable =
    let firstChannel = minBound :: Channel
        lastChannel  = maxBound :: Channel
        numChannels = fromEnum lastChannel - fromEnum firstChannel + 1
    in if length infoList == numChannels
               && map fst infoList == [firstChannel..lastChannel]  -- also requires sorted
           then V.fromListN numChannels (map snd infoList)
           else error "Invalid infoList in Import.hs"

networkChanList :: V.Vector [Channel]
networkChanList =
    let firstNetwork = minBound :: Network
        lastNetwork  = maxBound :: Network
        numNetworks = fromEnum lastNetwork - fromEnum firstNetwork + 1
    in V.fromListN numNetworks [map fst (filter ((== netw) . ciNetwork . snd) infoList)
                               | netw <- [firstNetwork..lastNetwork]]

channelsForNetwork :: Network -> [Channel]
channelsForNetwork netw = networkChanList V.! fromEnum netw

lookupInfo :: Channel -> ChanInfo
lookupInfo ch = infoTable V.! fromEnum ch

chanNetwork :: Channel -> Network
chanNetwork = ciNetwork . lookupInfo

-- | Pretty print a channel in a human-representation.
prettyChan :: Channel -> String
prettyChan = ciPretty . lookupInfo

-- | Pretty print a channel in a human-representation, prefixed by its network.
prettyChanWithNetwork :: Channel -> String
prettyChanWithNetwork ch = showNetwork (chanNetwork ch) ++ prettyChan ch

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
