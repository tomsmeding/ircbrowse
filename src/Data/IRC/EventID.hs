{-# LANGUAGE MultiWayIf #-}
module Data.IRC.EventID (
    DayCode,
    toDayCode,
    fromDayCode,
    EventId,
    packEventId,
    unpackEventId,
) where

import Ircbrowse.Types.Import

import Data.Bits
import Data.Time
import Data.Word


-- The ordering is chronological ordering.
newtype DayCode = DayCode Word32
  deriving (Show, Eq, Ord)

toDayCode :: Day -> DayCode
toDayCode day =
    let (y, m, d) = toGregorian day
    in DayCode (sum [wordCast 2 y `shiftL` 16
                    ,wordCast 1 m `shiftL` 8
                    ,wordCast 1 d])

fromDayCode :: DayCode -> Day
fromDayCode (DayCode code) =
    fromGregorian (fromIntegral (code `shiftR` 16))
                  (fromIntegral ((code `shiftR` 8) `mod` 256))
                  (fromIntegral (code `mod` 256))

newtype EventId = EventId Word64
  deriving (Eq)
  -- deriving (Show)

packEventId :: Channel -> DayCode -> Int -> EventId
packEventId chan (DayCode daycode) linenum =
    EventId (sum [wordCast 1 (showChanInt chan) `shiftL` (7 * 8)
                 ,wordCast 4 daycode `shiftL` (3 * 8)
                 ,wordCast 3 linenum])

unpackEventId :: EventId -> Maybe (Channel, DayCode, Int)
unpackEventId (EventId code) = do
    chan <- parseChanInt . fromIntegral $ code `shiftR` (7 * 8)
    let daycode = DayCode . fromIntegral $ (code `shiftR` (3 * 8)) `mod` (1 `shiftL` (4 * 8))
        linenum = fromIntegral $ code `mod` (1 `shiftL` (3 * 8))
    return (chan, daycode, linenum)

wordCast :: (Bits i, Integral i, FiniteBits t, Integral t) => Int -> i -> t
wordCast nbytes value =
    let result = fromIntegral value
    in if | finiteBitSize result < 8 * nbytes ->
                error "Invalid wordCast: target type too small"
          | maybe False (\bs -> bs <= 8 * nbytes) (bitSizeMaybe value) ->
                result
          | otherwise ->
              let condition = if finiteBitSize result == 8 * nbytes
                                  then fromIntegral result == value
                                  else result < (1 `shiftL` (8 * nbytes))
              in if condition
                     then result
                     else error "Invalid wordCast: input too large"
