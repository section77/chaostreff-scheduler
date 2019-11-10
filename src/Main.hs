{-# LANGUAGE ViewPatterns #-}
module Main where

import Protolude
import Paths_chaostreff_scheduler (version)
import Data.Version (showVersion)
import ChaostreffScheduler
import Args


main :: IO ()
main = parseArgs >>= run

run :: Args -> IO ()
run ShowVersion = putStrLn ("version: " <> showVersion version)
run (RunApp cfg) = runReaderT schedule cfg
