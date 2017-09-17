module Language.MSH.CodeGen.PrimaryInstance (
    genPrimaryInstance,
    genIdentityInstance,
    genParentalInstance
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateEnv
import Language.MSH.StateDecl
import Language.MSH.CodeGen.Shared (renameParent)
import Language.MSH.CodeGen.SharedInstance
import Language.MSH.CodeGen.Interop (parseType)

getBaseMonad :: Maybe String -> Type
getBaseMonad Nothing  = ConT $ mkName "Identity"
getBaseMonad (Just p) = renameParent (\n -> n ++ "M") $ parseType p

genPrimaryInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec
genPrimaryInstance env cls decs decl@(StateDecl {
    stateName    = name,
    stateParams  = vars,
    stateData    = ds,
    stateParentN  = mp,
    stateMethods = methods
}) = do
    let
        cxt = []
        cn  = mkName $ name ++ "Like"
        on  = mkName name
        sn  = mkName $ name ++ "State"
        bt  = getBaseMonad mp
        ty  = foldl AppT (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) (map (VarT . mkName) vars)
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
    invk <- genInvokeDef name
    mods <- genFields decl PrimaryInst
    ms   <- genMethods PrimaryInst decl decl methods name
    return $ InstanceD Nothing cxt ty ([fam,invk] ++ mods ++ ms)

genIdentityInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec
genIdentityInstance env cls decs decl@(StateDecl {
    stateName    = name,
    stateParams  = vars,
    stateData    = ds,
    stateParentN  = mp,
    stateMethods = methods
}) = do
    let
        cxt = []
        cn  = mkName $ name ++ "Like"
        on  = mkName name
        sn  = mkName $ name ++ "State"
        bt  = ConT $ mkName "Identity"
        ty  = foldl AppT (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) (map (VarT . mkName) vars)
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
    invk <- genInvokeDef name
    fs   <- genFields decl IdentityInst
    ms   <- genMethods IdentityInst decl decl methods name
    return $ InstanceD Nothing cxt ty ([fam,invk] ++ fs ++ ms)

genParentalInstance :: StateDecl -> StateDecl -> Q [Dec]
genParentalInstance sub parent = do
    let
        cxt = []
        cn  = mkName $ (stateName parent) ++ "Like"
        on  = mkName (stateName sub)
        sn  = mkName $ (stateName sub) ++ "State"
        bt  = getBaseMonad (stateParentN sub)
        -- TODO: not sure if the parameters should be from the parent or inferred from the parent type?
        ps  = map (VarT . mkName) (stateParams parent)
        ty  = foldl AppT (ConT cn) ([ConT on, ConT sn, bt] ++ ps)
        idty = foldl AppT (ConT cn) ([ConT on, ConT sn, ConT $ mkName "Identity"] ++ ps)
    fs <- genFields parent SecondaryInst
    ms <- genMethods SecondaryInst parent sub (stateMethods sub) (stateName parent)
    ifs <- genFields parent IdentityInst
    ims <- genMethods IdentityInst parent sub (stateMethods sub) (stateName parent)
    rs <- case stateParent parent of
        Nothing  -> return []
        (Just p) -> genParentalInstance sub p
    return $ [InstanceD Nothing cxt ty (fs ++ ms)
           , InstanceD Nothing cxt idty (ifs ++ ims)] ++ rs
