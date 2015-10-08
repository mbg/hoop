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

genParentPattern :: Name -> Name -> StateDecl -> Pat 
genParentPattern pd pp p
    | isBaseClass p = ConP (mkName $ stateName p ++ "Data") [VarP pd]
    | otherwise     = ConP (mkName $ stateName p ++ "End") [VarP pp, VarP pd]

genParentCtr :: Name -> Name -> StateDecl -> Exp -> Exp
genParentCtr pd pp p s
    | isBaseClass p = 
        foldl AppE (ConE (mkName $ stateName p ++ "Start")) [VarE pd, s]
    | otherwise     = 
        foldl AppE (ConE (mkName $ stateName p ++ "Middle")) [VarE pp, VarE pd, s]

-- downcast (CEnd )
genCastFromEnd :: StateDecl -> Q Clause 
genCastFromEnd (StateDecl { stateName = name, stateParent = Just p }) = do 
    d  <- newName "d"   -- represents the data of this object
    pd <- newName "pd"  -- represents the data of the parent
    pp <- newName "pp"  -- represents the parent of the parent
    let
        ctrName = mkName $ name ++ "End"

        parPat  = genParentPattern pd pp p

        exp     = AppE (ConE $ mkName $ name ++ "Data") (VarE d)
        pattern = ConP ctrName [parPat, VarP d]
        body    = genParentCtr pd pp p exp
    return $ Clause [pattern] (NormalB body) []

genCastFromMid :: StateDecl -> Q Clause 
genCastFromMid (StateDecl { stateName = name, stateParent = Just p }) = do 
    d  <- newName "d"   -- represents the data of this object
    ss <- newName "s"   -- represents the delta-object of the child
    pd <- newName "pd"  -- represents the data of the parent
    pp <- newName "pp"  -- represents the parent of the parent
    let
        ctrName = mkName $ name ++ "Middle"

        parPat  = genParentPattern pd pp p

        exp     = foldl AppE (ConE $ mkName $ name ++ "Start") [VarE d, VarE ss]
        pattern = ConP ctrName [parPat, VarP d, VarP ss]
        body    = genParentCtr pd pp p exp
    return $ Clause [pattern] (NormalB body) [] 

-- | `genDowncastClauses s' generates the clauses for the `downcast'
--   function in an instance of `Cast' for state class `s'.
genDowncastClauses :: StateDecl -> Q [Clause]
genDowncastClauses s = do
    castFromEnd <- genCastFromEnd s
    castFromMid <- genCastFromMid s
    case stateMod s of 
        Nothing       -> return [castFromMid, castFromEnd]
        Just Abstract -> return [castFromMid]
        Just Final    -> return [castFromEnd]

-- | `genCastInstance s' generates an instance of the `Cast' typeclass for
--   state class `s' if `s' is not a base class.
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
        return $ [InstanceD [] (AppT (AppT ct ty) (parseType (stateName p))) [dwn]]


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