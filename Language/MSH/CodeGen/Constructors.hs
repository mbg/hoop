module Language.MSH.CodeGen.Constructors (
    genConstructors
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.Constructor
import Language.MSH.StateDecl
import Language.MSH.StateEnv
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop

{-
    Constructors
-}

genCtrParams :: StateDecl -> Q [(String, Name)]
genCtrParams (StateDecl {
    stateName = name,
    stateData = ds
}) = mapM (\(n,_) -> newName n >>= \v -> return (n,v)) (getFields ds)

-- | This is a hack to change the names of type variables in imported types from
--   unique names to unqualified names
unqualifyName :: Name -> Name
unqualifyName (Name occ flavour) = case flavour of
    NameU _ -> Name occ NameS
    _       -> Name occ flavour

unqualifyBndr :: TyVarBndr -> TyVarBndr
unqualifyBndr (PlainTV n)    = PlainTV (unqualifyName n)
unqualifyBndr (KindedTV n k) = KindedTV (unqualifyName n) k

--unqualifyPred :: Pred -> Pred
--unqualifyPred (AppT (AppT EqualityT a) b) = AppT (AppT EqualityT (normaliseType a)) (normaliseType b)
--unqualifyPred (ClassP n ts) = foldl AppT (ConT n) (map normaliseType ts)

normaliseType :: Type -> Type
normaliseType (ForallT bs ctx t) = ForallT (map unqualifyBndr bs) (map normaliseType ctx) (normaliseType t)
normaliseType (AppT f a) = AppT (normaliseType f) (normaliseType a)
normaliseType (SigT t k) = SigT (normaliseType t) k
normaliseType (VarT n) = VarT (unqualifyName n)
normaliseType t = t

genPCtrParams :: StateEnv -> String -> Q [(Type,Name)]
genPCtrParams env pn = case M.lookup pn env of
    (Just s) -> do
        ts <- getFieldTypes $ stateData s
        mapM (\(n,t) -> newName n >>= \n' -> return (t,n')) ts
    Nothing  -> do
        mn <- lookupValueName $ "_mk" ++ pn
        case mn of
            Nothing  -> fail $ "Constructor for `" ++ pn ++ "' is not in scope."
            (Just n) -> do
                (VarI _ t _) <- reify n
                mapM (\t -> newName "arg" >>= \n -> return (t,n)) (typeArgs $ normaliseType t)

genStateExpr :: StateDecl -> [(String, Name)] -> Exp
genStateExpr (StateDecl {
    stateName = name,
    stateData = ds
}) vs = RecConE (mkName $ "Mk" ++ name ++ "State") baseFs
    where
        baseFs = [(mkName $ "_" ++ name ++ "_" ++ n, VarE v) | (n,v) <- vs]

-- | Generates the internal constructor `_mkS' for a class `S'.
genBaseConstructor :: StateEnv -> StateDecl -> Q StateCtr
genBaseConstructor env s@(StateDecl { stateName = name, stateParentN = mp, stateData = ds }) = do
    vs <- genCtrParams s
    ts <- map snd <$> getFieldTypes ds
    let
        baseName = mkName $ "_mk" ++ name
        stateExp = genStateExpr s vs
        ps       = map (VarP . snd) vs
    case mp of
        Nothing  -> do
            let
                cn  = mkName $ name ++ "Data"
                con = RecConE cn [(mkName $ "_" ++ name ++ "_data", stateExp)]
            return $ SCtr {
                sctrDec   = FunD baseName [Clause ps (NormalB con) []],
                sctrTypes = ts
            }
        (Just p) -> do
            let
                cn        = mkName $ name ++ "End"
                Name pn _ = parentName $ parseType p
                pctr      = "_mk" ++ occString pn
            pps <- genPCtrParams env (occString pn)
            let
                pvs       = map snd pps
                supExp    = VarE $ mkName pctr
                appSup    = appEs supExp (map VarE pvs)
                con       = RecConE cn [(mkName $ "_" ++ name ++ "_data",stateExp), (mkName $ "_" ++ name ++ "_sup",appSup)]
            return $ SCtr {
                sctrDec   = FunD baseName [Clause (map VarP pvs ++ ps) (NormalB con) []],
                sctrTypes = map fst pps ++ ts
            }

{-genSuperConstructor :: StateDecl -> Q Dec
genSuperConstructor (StateDecl m name vars p decls) = do
    let
        supName = mkName $ "_mk" ++ name ++ "_super"
        supFs   = [(mkName $ "_" ++ n, parseExp e) | (n,e) <- getFields decls]
        supExp  = RecConE (mkName $ name ++ "Start")
    return $ FunD supName [Clause [] (NormalB supExp) []]-}

genConstructors :: StateEnv -> StateDecl -> Q StateCtr
genConstructors env s =
    genBaseConstructor env s
