module Language.MSH.CodeGen.Methods (
    genMethods
) where

import qualified Data.Map as M

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.StateEnv
import Language.MSH.CodeGen.Shared 
import Language.MSH.CodeGen.Inheritance

{-
    Methods
-}



-- | Generates a method belonging to a state class.
genMethod :: StateEnv -> StateDecl -> String -> [String] -> Dec -> Q [Dec]
genMethod env decl n vars (SigD name ty) 
    | isAbstract name decl = trace (nameBase name ++ " is abstract in " ++ show decl) $ return []
    | otherwise           = do
        o <- newName "o"
        s <- newName "s"
        m <- newName "m"
        let
            n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
            svs = appN (VarT s) vars
            stt = AppT (AppT (ConT (mkName "StateT")) svs) (VarT m)
            tvs = [PlainTV o, PlainTV s, PlainTV m]
            cxt = [foldl AppT (ConT $ mkName $ n ++ "Like") ([VarT o, VarT s, VarT m] ++ map (VarT . mkName) vars)]
        return [
            SigD n' $ unwrapForalls ty $ ForallT tvs cxt $ wrapMethodType False (\rt -> AppT stt rt) ty]
genMethod env decl n vars (ValD (VarP name) body wh) = do
    let
        n' = mkName $ "_" ++ n ++ "_" ++ nameBase name
    return [ValD (VarP n') body wh]
genMethod env decl n vars (FunD name cs) = do
    let
        n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
    return [FunD n' cs]
genMethod env decl n vars dec = fail $ 
    "Unsupported type of definition within a state class:\n" ++ show dec

-- | Generates methods for a state class.
genMethods :: StateEnv -> StateDecl -> String -> [String] -> [Dec] -> Q [Dec]
genMethods env decl n vars ds = do
    concat `fmap` mapM (genMethod env decl n vars) ds

