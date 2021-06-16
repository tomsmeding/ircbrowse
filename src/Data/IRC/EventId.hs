{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DerivingVia #-}
module Data.IRC.EventId (
    DayCode,
    toDayCode, fromDayCode,
    EventId,
    packEventId, unpackEventId,
    serialiseEventId, deserialiseEventId,
) where

import Ircbrowse.Types.Import

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Bits
import Data.Time
import Data.Word
import Text.Read (readMaybe)


-- | The ordering is chronological ordering.
newtype DayCode = DayCode Word32
  deriving (Show, Eq, Ord)
  deriving (NFData) via Word32

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

-- | The 'Ord' ordering is: channel, daycode, linenum
newtype EventId = EventId Word64
  deriving (Eq, Ord)
  -- deriving (Show)
  deriving (NFData) via Word64

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

serialiseEventId :: EventId -> String
serialiseEventId (EventId num) = show num

deserialiseEventId :: String -> Maybe EventId
deserialiseEventId s = do
    num <- readMaybe s
    -- Check for a valid channel by unpacking
    (chan, daycode, linenum) <- unpackEventId (EventId num)
    -- Check that there aren't any excess 1-bits
    guard (packEventId chan daycode linenum == EventId num)
    -- The day code and line number are valid by definition, so no need to check those.
    return (EventId num)

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
