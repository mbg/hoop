module Language.MSH.StateDecl (
    module Language.MSH.MethodTable,

    StateMod(..),
    StateMemberDecl(..),
    StateDecl(..),

    isBaseClass,
    isAbstractClass,
    isFinalClass
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.MethodTable

data StateMod = Abstract | Final deriving Show

data StateMemberDecl = StateDataDecl {
    stateDataName   :: String,
    stateDataExpr   :: Maybe String,
    stateDataType   :: String
} deriving Show

data StateDecl = StateDecl {
    stateMod     :: Maybe StateMod,
    stateName    :: String,
    stateParams  :: [String],
    stateParentN :: Maybe String,
    stateParent  :: Maybe StateDecl,
    stateData    :: [StateMemberDecl],
    stateBody    :: [Dec],
    stateMethods :: MethodTable
} deriving Show

isBaseClass :: StateDecl -> Bool
isBaseClass (StateDecl { stateParentN = Nothing } ) = True
isBaseClass _ = False

isAbstractClass :: StateDecl -> Bool
isAbstractClass (StateDecl { stateMod = Just Abstract }) = True
isAbstractClass _ = False

isFinalClass :: StateDecl -> Bool
isFinalClass (StateDecl { stateMod = Just Final }) = True
isFinalClass _ = False