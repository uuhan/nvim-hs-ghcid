{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Neovim.Ghcid
Description :  Ghcid plugin
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Ghcid
    where

import Neovim

import Neovim.Ghcid.Plugin
import qualified Data.Map as Map

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = do
    _ <- vim_command "sign define GhcidWarn text=>> texthl=Search"
    _ <- vim_command "sign define GhcidErr text=!! texthl=ErrorMsg"
    wrapPlugin Plugin
        { exports = []
        , statefulExports =
            [ StatefulFunctionality
                { readOnly = ()
                , writable = GhcidState Map.empty []
                , functionalities =
                  [ $(command'  'ghcidStart)   ["async", "!"]
                  , $(command'  'ghcidStop)    ["async"]
                  , $(command'  'ghcidRestart) ["async"]
                  , $(command'  'ghcidExec)    ["async", "!"]
                  , $(function' 'ghcidRun)     Sync
                  ]
                }
            ]
        }
