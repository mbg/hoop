module Language.MSH.CodeGen.Class where

import Control.Applicative ((<$>))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.StateEnv
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Inheritance

{-
    Type class
-}

data SCV = SCV {
    scvObject :: Name,
    scvState  :: Name,
    scvMonad  :: Name
}

-- | Generates the context for the type class.
genClassContext :: [String] -> Maybe Type -> SCV -> Q Cxt
genClassContext vars Nothing (SCV o s m) = do
    --return [foldl AppT (ConT $ mkName "Monad") [VarT m]]
    let
        cn = ConT $ mkName "Object"
        fa = appN (VarT o) vars
        --sa = foldl AppT (ConT $ mkName "Selector") [appN (VarT o) vars, appN (VarT s) vars, VarT m, VarT $ mkName "r"]
        sa = appN (VarT s) vars
        ma = VarT m
        --ma = ConT $ mkName "Identity"
    return [foldl AppT cn [fa, sa, ma]]
    --return [foldl AppT (ConT $ mkName "Object") [appN (VarT o) vars, appN (VarT s) vars, VarT m]]
genClassContext vars (Just p) (SCV o s m) = do
    return [foldl AppT (ConT pcname) vars]
        where
            (Name pn _) = parentName p
            pcname = mkName $ occString pn ++ "Like"
            vars   = [VarT o, VarT s, VarT m] ++ parentArgs p

-- | Generates the typing for the `invoke' function.
genInvokeDecl :: [String] -> String -> SCV -> Q Dec
genInvokeDecl tyvars c (SCV o s m) = do
    o' <- newName "o'"
    d' <- newName "d'"
    r  <- newName "r"
    let
        name  = mkName $ "_" ++ c ++ "_invoke"
        cname = mkName $ c ++ "Like"
        base  = AppT (AppT (ConT (mkName "StateT")) (appN (VarT s) tyvars)) (VarT m)
        ctx   = [foldl AppT (ConT cname) ([VarT o', VarT d', base] ++ [VarT $ mkName n | n <- tyvars])]
        ovs   = appN (VarT o) tyvars
        ovs'  = appN (VarT o') tyvars
        sigma = ovs' `arr` (ovs' `arr` AppT base (tuple [VarT r, ovs'])) `arr` ovs `arr` AppT (VarT m) (tuple [VarT r, ovs, ovs'])
        ty    = ForallT [PlainTV o', PlainTV d', PlainTV r] ctx sigma
    return $ SigD name ty

getterName :: String -> String
getterName n = "_get_" ++ n

setterName :: String -> String
setterName n = "_set_" ++ n

fieldType :: Type -> Type -> Name -> Type -> Type
fieldType ovs svs m ft =
    foldl AppT (ConT (mkName "Selector")) [PromotedT (mkName "Field"), ovs, svs, (VarT m), ft ]

genModDeclsFor :: SCV -> [String] -> StateMemberDecl -> Q [Dec]
genModDeclsFor (SCV o s m) vars (StateDataDecl field _ typ) = do
    let
        ft = parseType typ
        ovs = appN (VarT o) vars
        svs = appN (VarT s) vars
        stt = AppT (AppT (ConT (mkName "StateT")) svs) (VarT m)
        -- external getter
        getterT  = ovs `arr` AppT (VarT m) (tuple [ft, ovs])
        getter   = SigD (mkName (getterName field)) getterT
        -- internal getter
        getterT' = AppT stt ft
        getter'  = SigD (mkName (getterName field ++ "'")) getterT'
        -- external setter
        setterT  = ovs `arr` ft `arr` AppT (VarT m) (tuple [TupleT 0,ovs])
        setter   = SigD (mkName (setterName field)) setterT
        -- internal setter
        setterT' = ft `arr` AppT stt (TupleT 0)
        setter'  = SigD (mkName (setterName field ++ "'")) setterT'
        -- field
        fieldT   = fieldType ovs svs m ft
        fieldS   = SigD (mkName field) fieldT
    return [getter,getter',setter,setter',fieldS]

genModsDecls :: SCV -> [String] -> [StateMemberDecl] -> Q [Dec]
genModsDecls scv vars fields = do
    decls <- mapM (genModDeclsFor scv vars) fields
    return $ concat decls

splitMethodType :: Type -> ([Type], Type)
splitMethodType (ForallT tvs cxt t)          = splitMethodType t
splitMethodType (AppT (AppT ArrowT arg) ret) = (arg : args, ret')
    where
        (args,ret') = splitMethodType ret
splitMethodType rt = ([],rt)



methodType :: Type -> Type -> Name -> [Type] -> Type -> Type
methodType ovs svs m args rt = parameterise args st
    where
        st = foldl AppT (ConT (mkName "Selector")) [PromotedT (mkName "Method"), ovs, svs, VarT m, rt]



genMethodDecls' :: StateEnv -> Maybe String -> SCV -> [String] -> Name -> Type -> Q [Dec]
genMethodDecls' env mp (SCV o s m) vars name ty = do
    let
        ovs      = appN (VarT o) vars
        svs      = appN (VarT s) vars
        stt      = AppT (AppT (ConT (mkName "StateT")) svs) (VarT m)
        -- external
        inty     = ovs `arr` wrapMethodType False (\rt -> AppT (VarT m) (tuple [rt,ovs])) ty
        internal = SigD (mkName $ "_ecall_" ++ nameBase name) inty --(unwrapForalls ty exty)
        -- internal
        exty     = wrapMethodType False (\rt -> AppT stt rt) ty
        external = SigD (mkName $ "_icall_" ++ nameBase name) exty --(unwrapForalls ty inty)
        -- method
        (args,ret) = splitMethodType ty
        mty      = methodType ovs svs m args ret
        method   = SigD name mty
    ov <- isInherited env mp name
    if ov then return []
    else return [external, internal, method]

genMethodDecls :: StateEnv -> Maybe String -> SCV -> [String] -> Dec -> Q [Dec]
genMethodDecls env mp scv vars (SigD name ty) =
    genMethodDecls' env mp scv vars name ty
genMethodDecls _ _ _ _ _ = return []

genMethodsDecls :: StateEnv -> Maybe String -> SCV -> [String] -> [Dec] -> Q [Dec]
genMethodsDecls env mp scv vars ds = do
    decls <- mapM (genMethodDecls env mp scv vars) ds
    return $ concat decls

-- | Generates the type class for a state declaration
genStateClass :: StateEnv -> [TyVarBndr] -> [Dec] -> StateDecl -> Q Dec
genStateClass env tyvars fs (StateDecl {
        stateName = name,
        stateParams = vs,
        stateParentN = p,
        stateData = ds
    }) = do
        o   <- newName "o"
        s   <- newName "s"
        m   <- newName "m"
        let
            fam   = OpenTypeFamilyD $ TypeFamilyHead (mkName $ name ++ "St") ([PlainTV o] {- ++ tyvars -}) (KindSig (VarT $ mkName "p")) Nothing
            scv   = SCV o s m
            cname = mkName $ name ++ "Like"
            vars = [PlainTV o, PlainTV s, PlainTV m] ++ tyvars
            deps = [FunDep [o] [s], FunDep [s] [o]]
        cxt <- genClassContext vs (parseType <$> p) scv
        inv <- genInvokeDecl vs name scv
        mds <- genModsDecls scv vs ds
        ms  <- genMethodsDecls env p scv vs fs
        return $ ClassD cxt cname vars deps ([fam,inv] ++ mds ++ ms)
