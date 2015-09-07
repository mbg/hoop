module Language.MSH.CodeGen.Methods (
    genMethods
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.CodeGen.Shared 
import Language.MSH.CodeGen.Inheritance

{-
    Methods
-}

-- | Generates a method belonging to a state class.
genMethod :: StateEnv -> String -> [String] -> Dec -> Q Dec
genMethod env n vars (SigD name ty) = do
    o <- newName "o"
    s <- newName "s"
    m <- newName "m"
    let
        n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
        svs = appN (VarT s) vars
        stt = AppT (AppT (ConT (mkName "StateT")) svs) (VarT m)
        tvs = [PlainTV o, PlainTV s, PlainTV m]
        cxt = [foldl AppT (ConT $ mkName $ n ++ "Like") ([VarT o, VarT s, VarT m] ++ map (VarT . mkName) vars)]
    return $ SigD n' $ unwrapForalls ty $ ForallT tvs cxt $ wrapMethodType False (\rt -> AppT stt rt) ty
genMethod env n vars (ValD (VarP name) body wh) = do
    let
        n' = mkName $ "_" ++ n ++ "_" ++ nameBase name
    return $ ValD (VarP n') body wh
genMethod env n vars (FunD name cs) = do
    let
        n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
    return $ FunD n' cs
genMethod env n vars dec = fail $ "Unknown type of method definition:\n" ++ show dec

-- | Generates methods for a state class.
genMethods :: StateEnv -> String -> [String] -> [Dec] -> Q [Dec]
genMethods env n vars = mapM (genMethod env n vars)
