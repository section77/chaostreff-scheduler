{-# LANGUAGE FlexibleContexts #-}
module Git where

import Protolude
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Process (callProcess, readProcess)
import qualified Data.Text as T

import AppCfg


gitCheckout :: (MonadIO m, MonadReader AppCfg m) => m ()
gitCheckout = do
  repoUrl <- asks cfgRepoUrl
  workDir <- asks cfgWorkDir
  ifM (liftIO $ doesDirectoryExist $ workDir </> ".git")
    (putText "update website repo" >> git "pull" [repoUrl])
    (putText "clone website repo"  >> (liftIO $ callProcess "git" ["clone", toS repoUrl, workDir]))


gitCommitAndPush :: (MonadIO m, MonadReader AppCfg m) => m ()
gitCommitAndPush = do
  -- commit changes if necessary
  whenM (not . T.null <$> git' "status" ["-s"]) $
    git "add" ["-A"] >> git "commit" ["-q", "-m", "chaostreff-scheduler"]

  -- push changes if necesarry
  pushChanges <- asks cfgPushChanges
  whenM ((&&) (pushChanges == PushChanges) . not . T.null <$> git' "log" ["...origin"]) $
    git "push" []


git :: (MonadIO m, MonadReader AppCfg m) => Text -> [Text] -> m ()
git cmd args = void $ git' cmd args


git' :: (MonadIO m, MonadReader AppCfg m) => Text -> [Text] -> m Text
git' cmd args = do
  workDir <- toS <$> asks cfgWorkDir
  liftIO $ toS <$> readProcess "git" (["-C", workDir, toS cmd] ++ (fmap toS args)) ""
