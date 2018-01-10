{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{- |
Module      :  Neovim.Ghcid.Plugin
Description :  Ghcid quickfix integration plugin
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Ghcid.Plugin
    where

import           Data.Yaml                    hiding (Object)
import           GHC.Generics
import           Neovim
import           Neovim.BuildTool
import           Neovim.Quickfix              as Q
import           Neovim.User.Choice           (yesOrNo)
import           Neovim.User.Input

import           Language.Haskell.Ghcid       as Ghcid

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import           Data.Either                  (rights)
import           Data.List                    (group, sort)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (mapMaybe)
import           System.FilePath


-- | Simple data type containing a few information on how to start ghcid.
data ProjectSettings = ProjectSettings
        { rootDir :: FilePath
        -- ^ Project directory from which ghcid can be started successfully.
        --
        , cmd     :: String
        -- ^ Command to start a ghci session (usually @cabal repl@ or
        -- @stack ghci@).
        }
    deriving (Eq, Ord, Show, Generic)


instance ToJSON ProjectSettings
instance FromJSON ProjectSettings

type Mod = String

data GhcidState r = GhcidState
    { startedSessions :: Map FilePath (Ghci, Neovim r (GhcidState r) ())
    -- ^ A map from the root directory (see 'rootDir') to a 'Ghci' session and a
    -- release function which unregisters some autocmds and stops the ghci
    -- session.
    -- , omniHints       :: Map FilePath (Ghci, Map Mod [String])
    -- ^ A map for caching omnifunc hints by browse command
    , quickfixItems   :: [QuickfixListItem String]
    }


modifyStartedSessions :: (Map FilePath (Ghci, Neovim r (GhcidState r) ())
                          -> Map FilePath (Ghci, Neovim r (GhcidState r) ()))
                      -> Neovim r (GhcidState r) ()
modifyStartedSessions f = modify $ \s -> s { startedSessions = f (startedSessions s) }


-- | Start or update a ghcid session.
--
-- This will call 'determineProjectSettings' and ask you to confirm or overwrite
-- its proposed settings. If you prepend a bang, it acts as if you have
-- confirmed all settings.
ghcidStart :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidStart copts = do
    currentBufferPath <- errOnInvalidResult $ vim_call_function "expand" [ObjectBinary "%:p:h"]
    liftIO (determineProjectSettings' currentBufferPath) >>= \case
        Nothing -> void $
            yesOrNo "Could not determine project settings. This plugin needs a project with a .cabal file to work."
        Just s -> case bang copts of
                    Just True -> void $ startOrReload s
                    _ -> do
                      d <- askForDirectory
                              "Specify directory from which ghcid should be started."
                              (Just (rootDir s))
                      c <- askForString
                              "Specify the command to execute (e.g. \"ghci\")."
                              (Just (cmd s))

                      let s' = ProjectSettings d c
                      whenM (yesOrNo "Save settings to file?") .
                          liftIO . BS.writeFile (d </> "ghcid.yaml") $ encode s'
                      void $ startOrReload s


-- | Start a new ghcid session or reload the modules to update the quickfix
-- list.
startOrReload :: ProjectSettings -> Neovim r (GhcidState r) Ghci
startOrReload s@(ProjectSettings d c) = Map.lookup d <$> gets startedSessions >>= \case
    Nothing -> do
        (g, ls) <- liftIO $ startGhci c (Just d) (\_ _ -> return ())
        applyQuickfixActions $ loadToQuickfix ls
        void $ vim_command "cwindow"
        ra <- addAutocmd "BufWritePost" def (void $ startOrReload s) >>= \case
            Nothing ->
                return $ return ()

            Just (Left a) ->
                return a

            Just (Right rk) ->
                return $ Resource.release rk

        modifyStartedSessions $ Map.insert d (g,ra >> liftIO (stopGhci g))
        return g

    Just (ghci, _) -> do
        items <- loadToQuickfix <$> liftIO (reload ghci)
        applyQuickfixActions items
        void $ vim_command $ "cwindow " ++ show (length items)
        return ghci

-- | Run command from nvim side
ghcidExec :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidExec copts = do
    currentBufferPath <- errOnInvalidResult $ vim_call_function "expand" [ObjectBinary "%:p:h"]
    liftIO (determineProjectSettings' currentBufferPath) >>= \case
        Nothing -> void $
          yesOrNo "Could not determine project settings. This plugin needs a project with a .cabal file to work."
        Just s@ProjectSettings{..} ->
          Map.lookup rootDir <$> gets startedSessions >>= \case
            Nothing -> case bang copts of
                         Just True -> startOrReload s >>= react
                         _ -> whenM (yesOrNo "You need to start run GhcidStart for this projet!")
                                (startOrReload s >>= react)
            Just (ghcid, _) -> react ghcid
    where
      react ghcid = input "λ>" Nothing Nothing >>= \case
          Left err -> return ()
          Right (ObjectString s) -> do
            let command = BS8.unpack s
            res <- liftIO $ exec ghcid command
            let its = length res
            let height = min its 10
            unless (its == 0) $ do
              Neovim.nvim_command' $ "below " ++ show height ++ " split"
              Neovim.nvim_command' "enew"
              buf <- Neovim.nvim_get_current_buf'
              Neovim.buffer_set_option' buf "buftype" $ ObjectString "nofile"
              Neovim.nvim_command' "autocmd WinLeave <buffer> :bd"
              Neovim.buffer_set_lines' buf 0 1 False res

          Right _ -> return ()

ghcidRun :: String -> Neovim r (GhcidState r) String
ghcidRun command = do
    currentBufferPath <- errOnInvalidResult $ vim_call_function "expand" [ObjectBinary "%:p:h"]
    liftIO (determineProjectSettings' currentBufferPath) >>= \case
        Nothing -> "" <$
          yesOrNo "Could not determine project settings. This plugin needs a project with a .cabal file to work."
        Just s@ProjectSettings{..} ->
          Map.lookup rootDir <$> gets startedSessions >>= \case
            Nothing -> do
              yes <- yesOrNo "You need to start run GhcidStart for this projet!"
              if yes then startOrReload s >>= react
                     else return ""
            Just (ghcid, _) -> react ghcid
    where
      react ghcid = do
        res <- liftIO $ exec ghcid command
        -- remove tailing '\n'
        return $ reverse . dropWhile (== '\n') . reverse . unlines $ res

applyQuickfixActions :: [QuickfixListItem String] -> Neovim r (GhcidState r) ()
applyQuickfixActions qs = do
    fqs <- (nub' . rights . map bufOrFile) <$> gets quickfixItems
    modify $ \s -> s { quickfixItems = qs }
    forM_ fqs $ \f -> void . vim_command $ "sign unplace * file=" <> f
    setqflist qs Replace
    placeSigns qs
  where
    nub' = map head . group . sort


placeSigns :: [QuickfixListItem String] -> Neovim r st ()
placeSigns qs = forM_ (zip [(1::Integer)..] qs) $ \(i, q) -> case (lnumOrPattern q, bufOrFile q) of
    (Right _, _) ->
        -- Patterns not handled as they are not produced
        return ()

    (_, Left _) ->
        -- Buffer type not handled because i don't know how to pass that here
        -- and it is not produced.
        return ()

    (Left lnum, Right f) -> do
        let signType = case errorType q of
                Q.Error   -> "GhcidErr"
                Q.Warning -> "GhcidWarn"

        -- TODO What if the file name contains spaces?
        void . vim_command $ unwords
            [ "sign place", show i, "line=" <> show lnum
            , "name=" <> signType, "file=" <> f
            ]

-- | Stop a ghcid session associated to the currently active buffer.
ghcidStop :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidStop _ = do
    d <- errOnInvalidResult $ vim_call_function "expand" [ObjectBinary "%:p:h"]
    Map.lookupLE d <$> gets startedSessions >>= \case
        Nothing ->
            return ()
        Just (p,(_, releaseAction)) -> do
            modifyStartedSessions $ Map.delete p
            releaseAction


-- | Same as @:GhcidStop@ followed by @:GhcidStart!@. Note the bang!
ghcidRestart :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidRestart _ = do
    ghcidStop def
    ghcidStart def { bang = Just True }


loadToQuickfix :: [Load] -> [QuickfixListItem String]
loadToQuickfix = dropWarningsIfErrorsArePresent . mapMaybe f
  where
    f m@Message{} =
        Just $ (quickfixListItem
                    ((Right . loadFile) m)
                    ((Left . fst . loadFilePos) m))
                    { col = Just ((snd . loadFilePos) m, True)
                    , Q.text = (unlines . loadMessage) m
                    , errorType = case loadSeverity m of
                        Ghcid.Warning -> Q.Warning
                        _             -> Q.Error
                    }
    f _ = Nothing

    dropWarningsIfErrorsArePresent xs =
        case filter ((== Q.Error) . errorType) xs of
            []  -> xs
            xs' -> xs'


maybePluginConfig :: MonadIO io => Directory -> io (Maybe BuildTool)
maybePluginConfig d = fmap (const Custom)
    <$> mkFile (Just d) "ghcid.yaml"

-- | Determine project settings for a directory.
--
-- This will traverse through all parent directories and search for a hint on
-- how to start the ghcid background process. The following configurations will
-- be tried in this order:
--
-- * A @ghcid.yaml@ file which can be created with the @GhcidStart@ command
-- * A @stack.yaml@ file
-- * A @cabal.sandbox.config@ file
-- * A @\*.cabal@ file
--
-- Note that 'ghcidStart' prompts for confirmation unless you prepend a bang.
-- So, if you want to use your preferred settings, simply save them to the
-- @ghcid.yaml@ file and you're done.
determineProjectSettings' :: FilePath -> IO (Maybe ProjectSettings)
determineProjectSettings' dir = runMaybeT $ do
    ds <- MaybeT $ fmap thisAndParentDirectories <$> mkDirectory dir
    buildTool <- MaybeT $ determineProjectSettings (maybePluginConfig : defaultProjectIdentifiers) ds
    case buildTool of
      (Stack, d) -> return $ ProjectSettings (getDirectory d) "stack ghci"
      (Cabal _, d) -> return $ ProjectSettings (getDirectory d) "cabal repl"
      (Custom, d) -> do
          f <- MaybeT $ mkFile (Just d) "ghcid.yaml"
          MaybeT $ decode <$> BS.readFile (getFile f)
      _ -> MaybeT $ return Nothing


