module AppCfg where

import Protolude

newtype Year = Year Integer deriving Show
newtype Month = Month Int deriving Show
newtype MonthCount = MonthCount Int deriving Show

data PushChanges = PushChanges | NoPushChanges deriving (Show, Eq)

data AppCfg = AppCfg
  { cfgRepoUrl     :: Text
  , cfgWorkDir     :: FilePath
  , cfgPushChanges :: PushChanges
  , cfgRunForYear  :: Year
  , cfgRunForMonth :: Month
  , cfgMonthCount  :: MonthCount
  } deriving Show

