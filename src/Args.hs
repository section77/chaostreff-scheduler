module Args where

import Protolude hiding (option)
import Options.Applicative
import Paths_chaostreff_scheduler (version)
import Data.Version (showVersion)
import Text.Read
import Data.Time
import AppCfg


data Args =
    ShowVersion
  | RunApp AppCfg deriving Show


parseArgs :: IO Args
parseArgs = do
  (y, m) <- currentYearMonth
  execParser $ opts y m
  where opts y m = info (helper <*> args y m)
                 (    fullDesc
                   <> progDesc ("chaostreff-scheduler - schedule chaostreffs")
                   <> header   ("chaostreff-schedluer - version: " <> showVersion version))


currentYearMonth :: IO (Year, Month)
currentYearMonth = do
  (y, m, _) <- toGregorian . utctDay <$> getCurrentTime
  pure (Year y, Month m)


args :: Year -> Month -> Parser Args
args y m =    flag' ShowVersion (short 'v' <> long "version" <> help "print the app version")
          <|> RunApp <$> appCfg y m



appCfg :: Year -> Month -> Parser AppCfg
appCfg y m =     AppCfg
           <$> strOption   (short 'r' <> long "repo"
                            <> help "website src repository")
           <*> strOption   (short 'w' <> long "workdir"
                            <> help "website checkout directory")
           <*> flag PushChanges NoPushChanges (long "no-push"
                                               <> help "dont push changes to the src repo")
           <*> option auto (short 'y' <> long "year"
                            <> help "run for the given year" <> value y)
           <*> option auto (short 'm' <> long "month"
                             <> help "run for the given month" <> value m)
           <*> option auto (short 'c' <> long "month-count"
                            <> help "generate for the given month count" <> value (MonthCount 2))


instance Read MonthCount
  where readsPrec _ = fmap (first MonthCount) . reads

instance Read Year
  where readsPrec _ = fmap (first Year) . reads

instance Read Month
  where readsPrec _ = fmap (first Month) . reads
