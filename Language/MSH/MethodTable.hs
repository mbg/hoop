module Language.MSH.MethodTable where

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

{-
    Methods
-}

data MethodTable = MkMethodTable {
    methodSigs :: M.Map String Dec,
    methodDefs :: M.Map String Dec
} deriving Show

emptyMethodTable :: MethodTable
emptyMethodTable = MkMethodTable M.empty M.empty

addMethodSig :: Name -> Dec -> MethodTable -> MethodTable
addMethodSig name dec tbl = tbl { 
    methodSigs = M.insert (nameBase name) dec (methodSigs tbl) }

addMethodDef :: Name -> Dec -> MethodTable -> MethodTable
addMethodDef name dec tbl = tbl { 
    methodDefs = M.insert (nameBase name) dec (methodDefs tbl) }



isImplemented :: Name -> MethodTable  -> Bool
isImplemented n tbl = M.member (nameBase n) (methodDefs tbl) 

-- | `preProcessMethods ds' builds a value of type `MethodTable' from a list
--   of top-level declarations.
preProcessMethods :: [Dec] -> MethodTable
preProcessMethods ds = go emptyMethodTable ds
    where
        go tbl []                                  = tbl 
        go tbl (d@(SigD name ty)             : ds) = go (addMethodSig name d tbl) ds
        go tbl (d@(FunD name cs)             : ds) = go (addMethodDef name d tbl) ds 
        go tbl (d@(ValD (VarP name) body wh) : ds) = go (addMethodDef name d tbl) ds
        go tbl (d                            : ds) = go tbl ds

