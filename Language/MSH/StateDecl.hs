module Language.MSH.StateDecl (
    module Language.MSH.MethodTable,

    StateMod(..),
    StateObjCtr(..),
    StateMemberDecl(..),
    StateDecl(..),

    isBaseClass,
    isAbstractClass,
    isFinalClass,
    ctrsForClass
) where

--------------------------------------------------------------------------------

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.MethodTable

--------------------------------------------------------------------------------

-- | Enumerates class modifiers.
data StateMod = Abstract | Final deriving Show

-- | Enumerates object constructor types.
data StateObjCtr = DataCtr | StartCtr | MiddleCtr | EndCtr deriving Show

data StateMemberDecl = StateDataDecl {
    stateDataName   :: String,
    stateDataExpr   :: Maybe String,
    stateDataType   :: String
} deriving Show

data StateDecl = StateDecl {
    stateMod      :: Maybe StateMod,
    stateName     :: String,
    stateParams   :: [String],
    stateParentN  :: Maybe String,
    stateParentPs :: [String],
    stateParent   :: Maybe StateDecl,
    stateData     :: [StateMemberDecl],
    stateBody     :: [Dec],
    stateMethods  :: MethodTable
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

--isOverriden :: String -> StateDecl -> Bool
--isOverriden name (StateDecl { stateMethods = ms }) =

-- | `ctrsForClass dec' returns a list of object states for the state class
--   described by `dec'
ctrsForClass :: StateDecl -> [StateObjCtr]
ctrsForClass (StateDecl { stateParentN = p, stateMod = m }) = case p of
    Nothing -> case m of
        Nothing         -> [DataCtr, StartCtr]
        (Just Abstract) -> [StartCtr]
        (Just Final)    -> [DataCtr]
    (Just _) -> case m of
        Nothing         -> [DataCtr, StartCtr, MiddleCtr, EndCtr]
        (Just Abstract) -> [StartCtr, MiddleCtr]
        (Just Final)    -> [DataCtr, EndCtr]
