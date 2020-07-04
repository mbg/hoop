--------------------------------------------------------------------------------
-- Monadic State Hierarchies                                                  --
-- Copyright 2013-2019 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Language.Hoop.CodeGen.Methods (
    genMethods
) where

--------------------------------------------------------------------------------

import qualified Data.Map as M

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.StateDecl
import Language.Hoop.StateEnv
import Language.Hoop.MethodTable
import Language.Hoop.CodeGen.Shared 
import Language.Hoop.CodeGen.Inheritance

--------------------------------------------------------------------------------

genMethodBody :: Name -> Dec -> Dec 
genMethodBody name (ValD (VarP _) body wh) =  ValD (VarP name) body wh
genMethodBody name (FunD _ cs) = FunD name cs

genMethod' :: String -> String -> [String] -> Type -> Dec -> Q [Dec]
genMethod' className name vars ty def = do 
    o <- newName "o"
    s <- newName "s"
    m <- newName "m"
    let
        n'  = mkName $ "_" ++ className ++ "_" ++ name
        svs = appN (VarT s) vars
        stt = AppT (AppT (ConT (mkName "StateT")) svs) (VarT m)
        tvs = [PlainTV o, PlainTV s, PlainTV m] ++ [PlainTV (mkName v) | v <- vars]
        cxt = [foldl AppT (ConT $ mkName $ className ++ "Like") ([VarT o, VarT s, VarT m] ++ map (VarT . mkName) vars)]
    return [
        SigD n' $ unwrapForalls ty $ ForallT tvs cxt $ wrapMethodType False (\rt -> AppT stt rt) ty
     ,  genMethodBody n' def
     ]

-- | Generates a method belonging to a state class.
genMethod :: StateDecl -> String -> [String] -> (String, MethodEntry) -> Q [Dec]
genMethod decl n vars (name, entry)
    | abstractEntry entry = trace (name ++ " is abstract in " ++ show decl) $ return []
    | otherwise = case entry of 
        GenesisMethod _ (Just (SigD _ ty)) (Just def) -> 
            genMethod' n name vars ty def
        OverridenMethod (SigD _ ty) def -> 
            genMethod' n name vars ty def
        InheritedMethod _ (SigD _ ty) (Just def) ->
            genMethod' n name vars ty def
        _ -> error $ "[genMethod] Unexpected method table entry:\n " ++ show entry

-- genMethod env decl n vars (ValD (VarP name) body wh) = do
--     let
--         n' = mkName $ "_" ++ n ++ "_" ++ nameBase name
--     return [ValD (VarP n') body wh]
-- genMethod env decl n vars (FunD name cs) = do
--     let
--         n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
--     return [FunD n' cs]
-- genMethod env decl n vars dec = fail $ 
--     "Unsupported type of definition within a state class:\n" ++ show dec

-- | Generates methods for a state class.
-- genMethods :: StateEnv -> StateDecl -> String -> [String] -> [Dec] -> Q [Dec]
-- genMethods env decl n vars ds = do
--     concat `fmap` mapM (genMethod env decl n vars) ds

-- | `genMethods` Generates methods for a state class.
genMethods :: StateDecl -> String -> [String] -> Q [Dec]
genMethods decl n vars =
    concat <$> mapM (genMethod decl n vars) ms
    where ms = M.toList $ methods $ stateMethods decl

--------------------------------------------------------------------------------
