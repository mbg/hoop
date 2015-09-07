module Language.MSH.CodeGenMonad where

import Control.Monad.State

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.CodeGen.Shared

type CodeGen = StateT StateEnv Q