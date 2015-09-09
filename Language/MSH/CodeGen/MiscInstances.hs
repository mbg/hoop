module Language.MSH.CodeGen.MiscInstances (
    genMiscInstances
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.Constructor
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.NewInstance

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

genDowncastClauses :: StateDecl -> Q [Clause]
genDowncastClauses s@(StateDecl { stateName = name }) = return [] {-do
    obj <- newName "obj"
    let
        targetName  = mkName $ name ++ if isBaseClass (stateParent s) then "Start" else "Middle"
        target      = NormalB $ RecConE targetName []

        castFromEnd = Clause [VarP obj] target []
        castFromMid = Clause [VarP obj] target []
    case stateMod s of 
        Nothing       -> [castFromMid, castFromEnd]
        Just Abstract -> [castFromMid]
        Just Final    -> [castFromEnd]-}

-- TODO: instance body
genCastInstance :: StateDecl -> Q [Dec]
genCastInstance s@(StateDecl { 
    stateName = name, 
    stateParams = vars, 
    stateParent = mp
}) = case mp of
    Nothing  -> return []
    (Just p) -> do
        body <- genDowncastClauses s
        let
            ct  = ConT $ mkName "Cast"
            ty  = appN (ConT $ mkName name) vars
            dwn = FunD (mkName "downcast") body
        return $ [InstanceD [] (AppT (AppT ct ty) (parseType (stateName p))) [{-dwn-}]]


genMiscInstances :: StateDecl -> Dec -> StateCtr -> Q [Dec]
genMiscInstances decl dec ctr 
    | isAbstractClass decl = do
        d  <- genDataInstance decl dec
        cs <- genCastInstance decl
        return $ d : cs
    | otherwise = do
        d  <- genDataInstance decl dec
        n  <- genNewInstance ctr decl
        cs <- genCastInstance decl
        return $ d : n : cs