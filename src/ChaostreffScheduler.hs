{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module ChaostreffScheduler where

import Protolude
import Unsafe (unsafeHead)
import Data.Time
import Data.List (groupBy, concat)

import Lektor
import Event
import DayOfWeek

newtype Year = Year Integer
newtype Month = Month Int
newtype MonthCount = MonthCount Int



-- |
scheduleChaostreffs :: Year -> Month -> MonthCount -> IO ()
scheduleChaostreffs y m c = do
  template <- loadLektorEventTemplate "contents-chaostreff.tmpl"

  let dates = chaostreffDates y m c
      events = fmap (\d -> template { eventTitle = "Chaostreff", eventDate = d }) dates

  mapM_ createLektorCalendarEntry events



-- |
scheduleTechEvents :: Year -> Month -> MonthCount -> IO ()
scheduleTechEvents y m c = do
  template <- loadLektorEventTemplate "contents-tech-event.tmpl"

  let dates = techEventDates y m c
      events = fmap (\d -> template { eventTitle = "Tech-Event", eventDate = d }) dates

  mapM_ createLektorCalendarEntry events



-- |
-- >>> chaostreffDates (Year 2019) (Month 1) (MonthCount 2)
-- [2019-01-01 20:00:00 UTC,2019-01-15 20:00:00 UTC,2019-01-29 20:00:00 UTC,2019-02-12 20:00:00 UTC,2019-02-26 20:00:00 UTC]
chaostreffDates :: Year -> Month -> MonthCount -> [UTCTime]
chaostreffDates y m c = fmap withTimes . filterOdds . concat $ filter ((== Tuesday) . dayOfWeek) <$> range y m c
  where withTimes d = UTCTime d (timeOfDayToTime $ TimeOfDay 20 0 0)
        filterOdds = fmap snd . filter (odd . fst) . zip [1..]



-- |
-- >>> techEventDates (Year 2019) (Month 1) (MonthCount 4)
-- [2019-01-05 14:00:00 UTC,2019-02-02 14:00:00 UTC,2019-03-02 14:00:00 UTC,2019-04-06 14:00:00 UTC]
techEventDates :: Year -> Month -> MonthCount -> [UTCTime]
techEventDates y m c = fmap withTimes  $ unsafeHead <$> filter ((== Saturday) . dayOfWeek) <$> range y m c
  where withTimes d = UTCTime d (timeOfDayToTime $ TimeOfDay 14 0 0)



-- | creates a range of days starting with the given year and month over the given count of months
range :: Year -> Month -> MonthCount -> [[Day]]
range (Year y) (Month m) (MonthCount n) = take n $ groupByMonth $ days (fromGregorian y m 1)
  where days d = d : days (addDays 1 d)
        month (toGregorian -> (_, m, _)) = m
        groupByMonth = groupBy (\d1 d2 -> month d1 == month d2)
