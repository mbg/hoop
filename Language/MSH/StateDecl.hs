module Language.MSH.StateDecl where

data StateMod = Abstract | Final deriving Show

data StateMemberDecl = StateDataDecl {
    stateDataName   :: String,
    stateDataExpr   :: Maybe String,
    stateDataType   :: String
} deriving Show

data StateDecl = StateDecl {
    stateMod    :: Maybe StateMod,
    stateName   :: String,
    stateParams :: [String],
    stateParent :: Maybe String,
    stateData   :: [StateMemberDecl],
    stateBody   :: String
} deriving Show
