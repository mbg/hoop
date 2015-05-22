module Language.MSH.CodeGen.Constructors (
    genConstructors
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop

{-
    Constructors
-}

genStateExpr :: StateDecl -> Exp
genStateExpr (StateDecl { 
    stateName = name,
    stateData = ds 
}) = RecConE (mkName $ "Mk" ++ name ++ "State") baseFs
    where
        baseFs = [(mkName $ "_" ++ name ++ "_" ++ n, parseExp e) | (n,e) <- getFields ds]

genBaseConstructor :: StateDecl -> Q Dec
genBaseConstructor s@(StateDecl m name vars Nothing ds decls) = do
    let
        baseName = mkName $ "_mk" ++ name
        stateExp = genStateExpr s
        baseExp  = RecConE (mkName $ name ++ "Data") [(mkName $ "_" ++ name ++ "_data",stateExp)]
    return $ FunD baseName [Clause [] (NormalB baseExp) []]
genBaseConstructor s@(StateDecl m name vars (Just p) ds decls) = do
    let
        baseName = mkName $ "_mk" ++ name
        stateExp = genStateExpr s
        Name pn _ = parentName $ parseType p
        supExp   = VarE $ mkName $ "_mk" ++ occString pn
        baseExp  = RecConE (mkName $ name ++ "End") [(mkName $ "_" ++ name ++ "_data",stateExp), (mkName $ "_" ++ name ++ "_sup",supExp)]
    return $ FunD baseName [Clause [] (NormalB baseExp) []]

{-genSuperConstructor :: StateDecl -> Q Dec
genSuperConstructor (StateDecl m name vars p decls) = do
    let
        supName = mkName $ "_mk" ++ name ++ "_super"
        supFs   = [(mkName $ "_" ++ n, parseExp e) | (n,e) <- getFields decls]
        supExp  = RecConE (mkName $ name ++ "Start")
    return $ FunD supName [Clause [] (NormalB supExp) []]-}

genConstructors :: StateDecl -> Q [Dec]
genConstructors s@(StateDecl m name vars mp ds decls) = do
    base  <- genBaseConstructor s
    return [base]
    {-super <- genSuperConstructor s
    case m of
        Nothing         -> return [base, super]
        (Just Final)    -> return [base]
        (Just Abstract) -> return [super]-}