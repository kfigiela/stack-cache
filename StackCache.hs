{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
#!/usr/bin/env stack
-- stack --resolver lts-16.3 script --package yaml --package aeson --package universum --package shelly --package optparse-applicative --package text

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Universum
import Shelly hiding (whenM, unlessM, cd, rm_rf, test_e, test_d, mv, mkdir_p, lsT, test_s)
import qualified Shelly as S
import Options.Applicative hiding (command)
import Options.Applicative.Help ((.$.), Doc)
import qualified Options.Applicative as O

import qualified Data.Yaml as Y
import qualified Data.Text as T
import System.Environment (getEnv)
import GHC.IO (unsafeDupablePerformIO)
import qualified System.Posix.Files as Posix
import qualified System.Posix.Directory as Posix
import qualified GHC.IO.Exception as IOE
default (Text)

main :: IO ()
main =
  execParser (info (opts <**> helper) (
        fullDesc
     <> header "stack-cache – manage stack-work cache during SCM branch switches"
     <> footerDoc (Just doc)
   ))
   >>= shelly

doc :: Doc
doc = "SETUP: "
  .$. "1. Add stack-cache to $PATH"
  .$. "2. Perform stack-cache init from the directory with stack.yaml"
  .$. "3. Add post-checkout git hook to call `stack-cache relink` on branch change"
  .$. ""
  .$. "4. Make sure to ignore stack-cache directories:"
  .$. "$ git config --global core.excludesfile ~/.gitignore"
  .$. "$ echo .stack-work-cache >> ~/.gitignore"
  .$. "$ echo .stack-work >> ~/.gitignore"
  .$. ""
  .$. "5. Periodically run stack-cache clean-all to free some disk space"

opts :: Parser (Sh ())
opts = subparser
 (  O.command "init"                (info (pure initCache)                                               (progDesc "Create initial directory structure"))
 <> O.command "relink"              (info (pure $ check >> relink)                                       (progDesc "Relink stack-work from current branch cache"))
 <> O.command "check"               (info (pure check)                                                   (progDesc "Check if stack-cache is properly inited (do before switching branches)"))
 <> O.command "clone"               (info ((clone . Branch) <$> argument str (metavar "BRANCH"))         (progDesc "Clone stack-work from other branch cache"))
 <> O.command "clean-branch"        (info ((cleanBranch . Branch) <$> argument str (metavar "BRANCH"))   (progDesc "Delete sepecified branch cache"))
 <> O.command "clean-current"       (info (pure $ withCurrentBranch cleanBranch)                      (progDesc "Remove current stack-work, use relink to copy from master"))
 <> O.command "clean-all"           (info (pure $ cleanAll)                                              (progDesc "Deletes cache of all branches except current branch and master"))
 )

check :: Sh ()
check = do
  withCurrentBranch $ \currentBranch -> do
    results <- forAllPackages $ \package -> sub $ do
      cd package
      exists <- test_e stackWork
      isSymlink <- test_s stackWork
      case (exists, isSymlink) of
        (True, False) -> pure True
        _ -> pure False

    let failed = any snd results
    unless failed $ putTextLn "Symlinks OK"
    when failed $ do
      putTextLn ".stack-work for some packages is not a symlink, aborting relink – please do stack-cache init, affected packages:"
      forM_ (fmap fst . filter snd $ results) (putTextLn . (" - "<>))
      errorExit "stack-cache is not properly initialized"

relink :: Sh ()
relink = do
  withCurrentBranch $ \currentBranch -> do
    forAllPackages_ $ \package -> sub $ do
      cd package
      exists <- test_e stackWork
      isSymlink <- test_s stackWork
      case (exists, isSymlink) of
        (False, _) -> pass
        (True, True) -> cmd "rm" stackWork
        (True, False) -> errorExit $ ".stack-work for " <> package <> " is not a symlink"

      cacheExists <- test_d_not_l (cacheDir currentBranch)
      masterExists <- test_d_not_l $ cacheDir master
      unless cacheExists $ do
        if masterExists
          then do
            putText "C"
            copyCoW (cacheDir master) (cacheDir currentBranch)
          else do
            mkdir_p $ cacheDir currentBranch
      ln_s (cacheDir currentBranch) stackWork

clone :: Branch -> Sh ()
clone fromBranch = do
  withCurrentBranch $ \currentBranch -> do
    cleanBranch currentBranch
    forAllPackages_ $ \package -> sub $ do
      cd package
      copyCoW (cacheDir fromBranch) (cacheDir currentBranch)

initCache :: Sh ()
initCache = do
  withCurrentBranch $ \currentBranch -> do
    forAllPackages_ $ \package -> sub $ do
      cd package
      mkdir_p cacheRoot
      whenM (test_d_not_l stackWork) $
        unlessM (test_d_not_l $ cacheDir currentBranch) $
          mv stackWork $ cacheDir currentBranch
      rm_rf stackWork
    relink
    forAllPackages_ $ \package -> sub $ do
      cd package
      ignoreBackups ".stack-work-cache"
      ignoreBackups stackWork


cleanBranch :: Branch -> Sh ()
cleanBranch branch = do
  forAllPackages_ $ \package -> sub $ do
    cd package
    rm_rf (cacheDir branch)

cleanAll :: Sh ()
cleanAll = do
  currentBranch <- getCurrentBranch
  cleanAllExcept $ catMaybes [currentBranch, Just master]

cleanAllExcept :: [Branch] -> Sh ()
cleanAllExcept branches = do
  let ignoreDirs = cacheDir <$> branches
  forAllPackages_ $ \package -> sub $ do
    cd package
    allDirs <- lsT cacheRoot
    let dirsToRemove = filter (\dir -> not (dir `elem` ignoreDirs)) allDirs
    forM_ dirsToRemove $ \dir -> do
      rm_rf dir

newtype Branch = Branch { unBranch :: Text }
  deriving newtype (Show, Eq, IsString)
master = Branch "master"

sanitizeBranch :: Branch -> Text
sanitizeBranch (Branch branch) = T.strip $ T.replace "/" "_" branch

cacheDir :: Branch -> Text
cacheDir branch = cacheRoot <> "/" <> sanitizeBranch branch

cacheRoot :: Text
cacheRoot =  "./.stack-work-cache/branch"

stackWork :: Text
stackWork = "./.stack-work"

data StackYAML = StackYAML { packages :: [Text] } deriving (Generic, Y.FromJSON)

allPackages :: MonadIO m => m [Text]
allPackages = packages <$> liftIO (Y.decodeFileThrow "stack.yaml")

forAllPackages_ :: MonadIO m => (Text -> m ()) -> m ()
forAllPackages_ = void . forAllPackages

forAllPackages :: MonadIO m => (Text -> m a) -> m [(Text, a)]
forAllPackages action = do
  packages <- allPackages
  results <- forM packages $ \package -> do
    result <- action package
    putText "."
    pure (package, result)
  putText "\n"
  pure results

git = cmd "/usr/bin/env" "git"
ln_s = cmd "ln" "-s" :: Text -> Text -> Sh ()

getCurrentBranch :: Sh (Maybe Branch)
getCurrentBranch = do
  refName <- silently $ Branch <$> git "rev-parse" "--abbrev-ref" "HEAD" --  git "symbolic-ref" "HEAD"
  pure $ if refName /= "HEAD"
    then Just refName
    else Nothing

withCurrentBranch :: (Branch -> Sh ()) -> Sh ()
withCurrentBranch action = do
  branchNameMb <- getCurrentBranch
  case branchNameMb of
    Just branchName -> action branchName
    Nothing -> putTextLn "Not on branch, doing nothing."

ignoreBackups :: Text -> Sh ()
ignoreBackups = when isMacOS . cmd "tmutil" "addexclusion"

copyCoW :: Text -> Text -> Sh ()
copyCoW from to = do
  when isMacOS $ cmd "cp" "-cpr" from to
  when isLinux $ cmd "cp" "--reflink=always" "-pr" from to

osType :: Text
osType = toText $ unsafeDupablePerformIO $ shelly $ silently $ cmd "uname"

isMacOS, isLinux :: Bool
isMacOS = T.isPrefixOf "Darwin" osType
isLinux = T.isPrefixOf "Linux" osType

-- Shelly API is a mess wrt. Text vs. String issue and FilePath aka String is not well supported by `cmd` in last version

cd = S.cd . toString
lsT = S.lsT . toString
rm_rf = S.rm_rf . toString
mkdir_p = S.mkdir_p . toString
mv f t = S.mv (toString f) (toString t)
test_e = fmap isJust . getFileStatus
test_d = fmap (maybe False Posix.isDirectory) . getFileStatus
test_s = fmap (maybe False Posix.isSymbolicLink) . getFileStatus

getFileStatus :: Text -> Sh (Maybe Posix.FileStatus)
getFileStatus = fmap toText . absPath . toString >=> \path -> liftIO $ do
  result <- try $ Posix.getSymbolicLinkStatus (toString path)
  case result of
    Left err | IOE.ioe_type err == IOE.NoSuchThing -> pure Nothing
    Left err -> IOE.ioError err
    Right status -> pure $ Just status

test_d_not_l :: Text -> Sh Bool
test_d_not_l path = do
  status <- getFileStatus path
  pure $ case status of
    Just status -> Posix.isDirectory status && not (Posix.isSymbolicLink status)
    Nothing -> False
