{-# LANGUAGE LambdaCase #-}
module DayOfWeek where

import Protolude
import Data.Time

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)


instance Enum DayOfWeek where

  toEnum n = case mod n 7 of
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    _ -> Saturday

  fromEnum = \case
    Monday -> 1
    Tuesday -> 2
    Wednesday -> 3
    Thursday -> 4
    Friday -> 5
    Saturday -> 6
    Sunday -> 7


dayOfWeek :: Day -> DayOfWeek
dayOfWeek = toEnum . fromInteger . (+) 3 . toModifiedJulianDay
