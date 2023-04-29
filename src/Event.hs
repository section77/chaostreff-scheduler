module Event where

import Protolude
import Data.Time

data Event = Event
  { eventTitle :: Text
  , eventDate  :: UTCTime
  , eventEnd   :: UTCTime
  , eventUrl   :: Text
  , eventText  :: Text
  } deriving Show
