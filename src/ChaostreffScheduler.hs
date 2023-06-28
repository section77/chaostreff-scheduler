{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module ChaostreffScheduler where

import Protolude
import Data.Time
import Data.List ((!!), groupBy, concat, head)

import Lektor
import Event
import AppCfg
import Git


schedule :: (MonadIO m, MonadReader AppCfg m) => m ()
schedule = do
  gitCheckout
  scheduleChaostreffs
  scheduleTechEvents
--  scheduleFreiesHacken
--  schedule3DDD
  gitCommitAndPush


-- |
scheduleChaostreffs :: (MonadIO m, MonadReader AppCfg m) => m ()
scheduleChaostreffs = do
  template <- loadLektorEventTemplate "contents-chaostreff.tmpl"
  days <- chaostreffDays
  mapM_ (createLektorCalendarEntry $ template { eventTitle = "Chaostreff"}) days



-- |
scheduleTechEvents :: (MonadIO m, MonadReader AppCfg m) => m ()
scheduleTechEvents = do
  template <- loadLektorEventTemplate "contents-tech-event.tmpl"
  days <- techEventDays
  mapM_ (createLektorCalendarEntry $ template { eventTitle = "Tech-Event"}) days


scheduleFreiesHacken :: (MonadIO m, MonadReader AppCfg m) => m ()
scheduleFreiesHacken = do
  template <- loadLektorEventTemplate "contents-freies-hacken.tmpl"
  days <- techEventDays
  mapM_ (createLektorCalendarEntry $ template { eventTitle = "Freies Hacken"}) days


schedule3DDD :: (MonadIO m, MonadReader AppCfg m) => m ()
schedule3DDD = do
  template <- loadLektorEventTemplate "contents-3ddd.tmpl"
  days <- dddDays
  mapM_ (createLektorCalendarEntry $ template { eventTitle = "3D-Drucker-Donnerstag"}) days


-- |
-- >>> runReaderT chaostreffDays $ AppCfg "" "" NoPushChanges (Year 2019) (Month 2) (MonthCount 2)
-- [2019-02-05,2019-02-12,2019-02-19,2019-02-26,2019-03-05,2019-03-12,2019-03-19,2019-03-26]
chaostreffDays :: (MonadIO m, MonadReader AppCfg m) => m [Day]
chaostreffDays = concat <$> (filter ((== Tuesday) . dayOfWeek)) <$$> range



-- |
-- >>> runReaderT techEventDays $ AppCfg "" "" NoPushChanges (Year 2019) (Month 1) (MonthCount 4)
-- [2019-01-05,2019-02-02,2019-03-02,2019-04-06]
techEventDays :: (MonadIO m, MonadReader AppCfg m) => m [Day]
techEventDays = (Data.List.head . filter ((== Saturday) . dayOfWeek)) <$$> range



-- | 'Freies Hacken' dates - every second (odd) month, the third saturday
freiesHackenDays :: (MonadIO m, MonadReader AppCfg m) => m [Day]
freiesHackenDays = filter isOddMonth <$> (third . filter isSaturday) <$$> range
  where isSaturday = (== Saturday) . dayOfWeek
        third xs = xs !! 2
        isOddMonth (toGregorian -> (_, m, _)) = odd m


-- | '3DDD' dates - every third thursday
-- >>> runReaderT dddDays $ AppCfg "" "" NoPushChanges  (Year 2021) (Month 8) (MonthCount 2)
-- [2021-08-19,2021-09-16]
dddDays :: (MonadIO m, MonadReader AppCfg m) => m [Day]
dddDays = (third . filter isThursday) <$$> range
  where isThursday = (== Thursday) . dayOfWeek
        third xs = xs !! 2



(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap


-- | creates a range of days grouped by month depending on the given config
range :: (MonadIO m, MonadReader AppCfg m) => m [[Day]]
range = do
  (Year y, Month m, MonthCount c) <- (,,) <$> asks cfgRunForYear <*> asks cfgRunForMonth <*> asks cfgMonthCount
  pure $ take c $ groupByMonth $ days (fromGregorian y m 1)
  where days d = d : days (addDays 1 d)
        month (toGregorian -> (_, m, _)) = m
        groupByMonth = groupBy (\d1 d2 -> month d1 == month d2)
