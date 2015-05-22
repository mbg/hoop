module Language.MSH.CodeGen.MiscInstances (
    genMiscInstances
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Shared

{-
    Misc. Instances
-}

-- | Generates a caluse which extracts the data from an object's constructor.
genObjectDataExtractor :: String -> Name -> Q Clause
genObjectDataExtractor n ctr = do
    d <- newName "d"
    let
        pat  = RecP ctr [(mkName $ "_" ++ n ++ "_data", VarP d)]
        body = VarE d
    return $ Clause [pat] (NormalB body) []

-- | Generates a function which extracts the data from an object's constructors.
genObjectInstanceDec :: String -> [Con] -> Q Dec 
genObjectInstanceDec n ctrs = do
    cs <- mapM (genObjectDataExtractor n) (map conName ctrs)
    return $ FunD (mkName "extractData") cs

-- | Generates an instance of the `Object' class.
genDataInstance :: StateDecl -> Dec -> Q Dec
genDataInstance (StateDecl { 
    stateName = name, 
    stateParams = vars
}) (DataD _ oname tyvars cs _) = do
    let
        ct = ConT $ mkName "HasData"
        ty = appN (ConT $ mkName name) vars
        dt = appN (ConT $ mkName $ name ++ "State") vars
    decs <- genObjectInstanceDec name cs 
    return $ InstanceD [] (AppT (AppT ct ty) dt) [decs]

-- TODO: instance body
genCastInstance :: StateDecl -> Q [Dec]
genCastInstance (StateDecl { 
    stateName = name, 
    stateParams = vars, 
    stateParent = mp
}) = case mp of
    Nothing  -> return []
    (Just p) -> do
        let
            ct = ConT $ mkName "Cast"
            ty = appN (ConT $ mkName name) vars
        return $ [InstanceD [] (AppT (AppT ct ty) (parseType p)) []]


genMiscInstances :: StateDecl -> Dec -> Q [Dec]
genMiscInstances decl dec = do
    d  <- genDataInstance decl dec
    cs <- genCastInstance decl
    return $ d : cs