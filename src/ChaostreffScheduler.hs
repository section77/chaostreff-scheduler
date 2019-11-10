{-# LANGUAGE FlexibleContexts #-}
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
import AppCfg
import Git


schedule :: (MonadIO m, MonadReader AppCfg m) => m ()
schedule = do
  gitCheckout
  scheduleChaostreffs
  scheduleTechEvents
  gitCommitAndPush


-- |
scheduleChaostreffs :: (MonadIO m, MonadReader AppCfg m) => m ()
scheduleChaostreffs = do
  template <- loadLektorEventTemplate "contents-chaostreff.tmpl"

  dates <- chaostreffDates
  let events = fmap (\d -> template { eventTitle = "Chaostreff", eventDate = d }) dates

  mapM_ createLektorCalendarEntry events



-- |
scheduleTechEvents :: (MonadIO m, MonadReader AppCfg m) => m ()
scheduleTechEvents = do
  template <- loadLektorEventTemplate "contents-tech-event.tmpl"

  dates <- techEventDates
  let events = fmap (\d -> template { eventTitle = "Tech-Event", eventDate = d }) dates

  mapM_ createLektorCalendarEntry events



-- |
-- >>> runReaderT chaostreffDates $ AppCfg "" "" NoPushChanges (Year 2019) (Month 2) (MonthCount 2)
-- [2019-02-05 20:00:00 UTC,2019-02-19 20:00:00 UTC,2019-03-05 20:00:00 UTC,2019-03-19 20:00:00 UTC]
chaostreffDates :: (MonadIO m, MonadReader AppCfg m) => m [UTCTime]
chaostreffDates = fmap withTime . concat <$> (filterOdds . filter ((== Tuesday) . dayOfWeek)) <$$> range
  where withTime d = UTCTime d (timeOfDayToTime $ TimeOfDay 20 0 0)
        filterOdds = fmap snd . filter (odd . fst) . zip [1..]



-- |
-- >>> runReaderT techEventDates $ AppCfg "" "" NoPushChanges (Year 2019) (Month 1) (MonthCount 4)
-- [2019-01-05 14:00:00 UTC,2019-02-02 14:00:00 UTC,2019-03-02 14:00:00 UTC,2019-04-06 14:00:00 UTC]
techEventDates :: (MonadIO m, MonadReader AppCfg m) => m [UTCTime]
techEventDates = (withTime <$> unsafeHead . filter ((== Saturday) . dayOfWeek)) <$$> range
  where withTime d = UTCTime d (timeOfDayToTime $ TimeOfDay 14 0 0)


(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap



-- | creates a range of days depending on the given config
range :: (MonadIO m, MonadReader AppCfg m) => m [[Day]]
range = do
  (Year y, Month m, MonthCount c) <- (,,) <$> asks cfgRunForYear <*> asks cfgRunForMonth <*> asks cfgMonthCount
  pure $ take c $ groupByMonth $ days (fromGregorian y m 1)
  where days d = d : days (addDays 1 d)
        month (toGregorian -> (_, m, _)) = m
        groupByMonth = groupBy (\d1 d2 -> month d1 == month d2)
