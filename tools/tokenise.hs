{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.List (tails)
import qualified Data.Map.Strict as Map


newtype Token = Token String
  deriving (Eq, Ord)

tokenLength :: Int
tokenLength = 4

log10 :: Floating a => a -> a
log10 x = log x / log 10

validToken :: Token -> Bool
validToken (Token s) = all (not . isSpace) s

transformToken :: Token -> Token
transformToken (Token s) = Token (map toLower s)

tokenise :: String -> [Token]
tokenise s =
    let n = length s
    in filter validToken $ map (transformToken . Token . take tokenLength) (take (n - tokenLength + 1) (tails s))

encodeIndexList :: [Int] -> LBS.ByteString
encodeIndexList = BSB.toLazyByteString . foldMap encodeInt
  where
    -- big-endian variable-width; most significant bit of byte is 1 if more
    -- bytes follow, 0 if last byte of variable-width number
    encodeInt :: Int -> BSB.Builder
    encodeInt n =
        let digits = reverse (takeWhile (> 0) (map (fromIntegral . (`mod` 128)) (iterate (`div` 128) n)))
        in case digits of
             [] -> BSB.word8 0
             _ -> foldMap (BSB.word8 . (+ 128)) (init digits) <> BSB.word8 (last digits)

main :: IO ()
main = do
    events <- zip [1::Int ..] . lines <$> readFile "../events_sample.txt"
    let tokens = Map.fromListWith (++) $
                    concatMap (\(i, s) -> map (,[i]) (tokenise s)) events
    -- mapM_ (\(_, lst) -> print (log10 (fromIntegral (length lst)) :: Double)) (Map.toList tokens)
    -- mapM_ (\(Token s, _) -> putStrLn s) (filter ((> 1000) . length . snd) (Map.toList tokens))
    print (sum . map LBS.length $ map (encodeIndexList . snd) (Map.toList tokens))
