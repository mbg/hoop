module Language.MSH.CodeGen.Methods (
    genMethods
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.CodeGen.Shared (appN, unwrapForalls, wrapMethodType)

{-
    Methods
-}

-- | Generates a method belonging to a state class.
genMethod :: String -> [String] -> Dec -> Q Dec
genMethod n vars (SigD name ty) = do
    o <- newName "o"
    s <- newName "s"
    m <- newName "m"
    let
        n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
        svs = appN (VarT s) vars
        stt = AppT (AppT (ConT (mkName "StateT")) svs) (VarT m)
        tvs = [PlainTV o, PlainTV s, PlainTV m]
        cxt = [ClassP (mkName $ n ++ "Like") ([VarT o, VarT s, VarT m] ++ map (VarT . mkName) vars)]
    return $ SigD n' $ unwrapForalls ty $ ForallT tvs cxt $ wrapMethodType False (\rt -> AppT stt rt) ty
genMethod n vars (ValD (VarP name) body wh) = do
    let
        n' = mkName $ "_" ++ n ++ "_" ++ nameBase name
    return $ ValD (VarP n') body wh
genMethod n vars (FunD name cs) = do
    let
        n'  = mkName $ "_" ++ n ++ "_" ++ nameBase name
    return $ FunD n' cs
genMethod n vars dec = error $ show dec

-- | Generates methods for a state class.
genMethods :: String -> [String] -> [Dec] -> Q [Dec]
genMethods n vars = mapM (genMethod n vars)
