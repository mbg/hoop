module Language.MSH.CodeGen.SharedInstance (
    ImplMode(..),

    genFields,
    genMethods,
    genInvokeDef,
    genInvoke,
    genRunStateT
) where

--------------------------------------------------------------------------------

import Debug.Trace

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.StateEnv
import Language.MSH.CodeGen.Shared (countTypeArgs)
import Language.MSH.CodeGen.Inheritance

--------------------------------------------------------------------------------

-- | Enumerates different member generation modes.
data ImplMode = PrimaryInst | SecondaryInst | IdentityInst
    deriving Show

{--------------------------------------------------------------------------
    Control flow
--------------------------------------------------------------------------}

genUndefined :: Exp
genUndefined = VarE $ mkName "undefined"

-- | `lifted' @exp wraps @exp in a call to @lift.
lifted :: Exp -> Exp
lifted = AppE (VarE $ mkName "lift")

-- | `composed` @f @g composes @f and @g.
composed :: Exp -> Exp -> Exp
composed f g = AppE (AppE (VarE $ mkName ".") f) g

-- | `genRunStateT f d' generates a call to `runStateT' where `f` is the
--   computation to be run and `d' is the initial state.
genRunStateT :: Exp -> Exp -> Exp
genRunStateT f d = AppE (AppE (VarE $ mkName "runStateT") f) d

-- | `genInvoke pn obj exp st' generates a call to `_pn_invoke' for some
--   state class named `pn' where `obj' is the base delta-object, `exp'
--   is the expression to run and `st' is the state(of what?)
genInvoke :: String -> Exp -> Exp -> Exp -> Exp
genInvoke pn obj exp st = foldl AppE (VarE invk_name) [obj, exp, st]
    where
        invk_name = mkName $ "_" ++ pn ++ "_invoke"

-- | Generates the implementation of the `_C_invoke' function.
--   The purpose of the `_C_invoke' functions is to allow a sub-class to
--   pass an arbitrary method to the super-class. It works as follows:
--
genInvokeDef :: String -> Q Dec
genInvokeDef n = do
    s <- newName "s"
    f <- newName "f"
    o <- newName "o"
    r <- newName "r"
    d' <- newName "d'"
    s' <- newName "s'"
    let
        name = mkName $ "_" ++ n ++ "_invoke"
        fn   = mkName $ "_" ++ n ++ "_data"
        ps   = [VarP s, VarP f, VarP o]
        runs = BindS (TupP [TupP [VarP r, VarP s'], VarP d']) (genRunStateT (AppE (VarE f) (VarE s)) (AppE (VarE $ mkName "extractData") (VarE o)))
        rets = AppE (VarE $ mkName "return") (TupE [VarE r, RecUpdE (VarE o) [(fn,VarE d')], VarE s'])
        body = NormalB $ DoE [runs, NoBindS rets]
    return $ FunD name [Clause ps body []]

genPrimaryClause :: StateDecl -> [Name] -> Exp -> (Exp -> Exp) -> StateObjCtr -> Q Clause
genPrimaryClause decl args call exp DataCtr = do
    d  <- newName "d"
    r  <- newName "r"
    d' <- newName "d'"
    let
        ctr  = mkName $ stateName decl ++ "Data"
        pat  = ConP ctr [VarP d] : map VarP args
        bpat = TupP [VarP r, VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, AppE (ConE ctr) (VarE d')])
        body = DoE [BindS bpat (genRunStateT call (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []
genPrimaryClause decl args call exp StartCtr = do
    d  <- newName "d"
    s  <- newName "s"
    r  <- newName "r"
    d' <- newName "d'"
    s' <- newName "s'"
    let
        ctr  = mkName $ stateName decl ++ "Start"
        pat  = ConP ctr [VarP d, VarP s] : map VarP args
        bpat = TupP [TupP [VarP r, VarP s'], VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, AppE (AppE (ConE ctr) (VarE d')) (VarE s')])
        body = DoE [BindS bpat (genRunStateT (exp (VarE s)) (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []
genPrimaryClause decl args call exp MiddleCtr = do
    p  <- newName "p"
    d  <- newName "d"
    s  <- newName "s"
    r  <- newName "r"
    d' <- newName "d'"
    s' <- newName "s'"
    let
        ctr  = mkName $ stateName decl ++ "Middle"
        pat  = ConP ctr [VarP p, VarP d, VarP s] : map VarP args
        bpat = TupP [TupP [VarP r, VarP s'], VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p, VarE d', VarE s']])
        body = DoE [BindS bpat (genRunStateT (exp (VarE s)) (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []
genPrimaryClause decl args call exp EndCtr = do
    p  <- newName "p"
    d  <- newName "d"
    r  <- newName "r"
    d' <- newName "d'"
    let
        ctr  = mkName $ stateName decl ++ "End"
        pat  = ConP ctr [VarP p, VarP d] : map VarP args
        bpat = TupP [VarP r, VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p, VarE d']])
        body = DoE [BindS bpat (genRunStateT call (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []

genIdentityClause :: StateDecl -> StateDecl -> [Name] -> Exp -> StateObjCtr -> Q (Maybe Clause)
genIdentityClause p (decl@StateDecl { stateParentN = Just pn }) args call MiddleCtr = do
    o  <- newName "o"
    p  <- newName "p"
    d  <- newName "d"
    s  <- newName "s"
    r  <- newName "r"
    p' <- newName "p'"
    d' <- newName "d'"
    s' <- newName "s'"
    let
        ctr  = mkName $ stateName decl ++ "Middle"
        pat  = AsP o (ConP ctr [VarP p, VarP d, VarP s]) : map VarP args
        opat = ConP ctr [WildP, VarP d', VarP s']
        bpat = TupP [VarP r, VarP p', opat]
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p', VarE d', VarE s']])
        body = DoE [BindS bpat (genInvoke pn (VarE o) call (VarE p)), NoBindS ret]
    return $ Just $ Clause pat (NormalB body) []
genIdentityClause p (decl@StateDecl { stateParentN = Just pn }) args call EndCtr = do
    o  <- newName "o"
    p  <- newName "p"
    d  <- newName "d"
    r  <- newName "r"
    p' <- newName "p'"
    d' <- newName "d'"
    s' <- newName "s'"
    let
        ctr  = mkName $ stateName decl ++ "End"
        pat  = AsP o (ConP ctr [VarP p, VarP d]) : map VarP args
        opat = ConP ctr [WildP, VarP d']
        bpat = TupP [VarP r, VarP p', opat]
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p', VarE d']])
        body = DoE [BindS bpat (genInvoke pn (VarE o) call (VarE p)), NoBindS ret]
    return $ Just $ Clause pat (NormalB body) []
genIdentityClause p decl args call ctr = trace ("Nothing" ++ show ctr) $ return Nothing

{--------------------------------------------------------------------------
    Fields
--------------------------------------------------------------------------}

lensName :: String -> String
lensName (x:xs) = toLower x : xs

genGetterBody :: Bool -> ImplMode -> String -> Name -> Exp
genGetterBody _ PrimaryInst  lens self = AppE (VarE $ mkName "use") (VarE $ mkName lens)
genGetterBody _ SecondaryInst  lens self = lifted (VarE self)
genGetterBody _ IdentityInst lens self = genUndefined -- genInvoke

genSetterBody :: Bool -> ImplMode -> String -> Name -> Exp
genSetterBody _ PrimaryInst  lens self = AppE (VarE $ mkName "assign") (VarE $ mkName lens)
genSetterBody _ SecondaryInst  lens self = (VarE $ mkName "lift") `composed` (VarE self)
genSetterBody _ IdentityInst lens self = genUndefined

genIntGetter :: Bool -> ImplMode -> String -> Name -> Dec
genIntGetter isBase mode lens name =
    FunD name [Clause [] (NormalB $ genGetterBody isBase mode lens name) []]

genIntSetter :: Bool -> ImplMode -> String -> Name -> Dec
genIntSetter isBase mode lens name =
    FunD name [Clause [] (NormalB $ genSetterBody isBase mode lens name) []]

genGetterClauses :: ImplMode -> StateDecl -> StateDecl -> String -> Q [Clause]
genGetterClauses PrimaryInst decl instanceOf name = trace ("Primary:" ++ name ++ show (ctrsForClass instanceOf)) $
    mapM (genPrimaryClause decl [] call exp) (ctrsForClass instanceOf)
    where
        call = VarE $ mkName $ name ++ "'"
        exp  = AppE (VarE $ mkName $ name)
genGetterClauses IdentityInst decl instanceOf name = trace ("Identity:" ++ name ++ show (ctrsForClass instanceOf)) $
    catMaybes <$> mapM (genIdentityClause decl instanceOf [] call) (ctrsForClass instanceOf)
    where
        call = VarE $ mkName $ name
genGetterClauses SecondaryInst decl instanceOf name = trace ("Secondary:" ++ name ++ show (ctrsForClass instanceOf)) $
    mapM (genPrimaryClause instanceOf [] call exp) (ctrsForClass instanceOf)
    where
        call = VarE $ mkName $ name ++ "'"
        exp  = AppE (VarE $ mkName $ name)-- TODO: error "SecondaryInst in genGetterClauses"

genSetterClauses :: ImplMode -> StateDecl -> StateDecl -> String -> Name -> Q [Clause]
genSetterClauses PrimaryInst decl instanceOf name var =
    mapM (genPrimaryClause decl [var] call exp) (ctrsForClass instanceOf)
    where
        call = AppE (VarE $ mkName $ name ++ "'") (VarE var)
        exp  = \s -> AppE (AppE (VarE $ mkName name) s) (VarE var)
genSetterClauses IdentityInst decl instanceOf name var = trace name $ do
    obj <- newName "obj"
    let
        call = LamE [VarP obj] $ foldl AppE (VarE $ mkName $ name) [VarE obj, VarE var]
    catMaybes <$> mapM (genIdentityClause decl instanceOf [var] call) (ctrsForClass instanceOf)
genSetterClauses SecondaryInst decl instanceOf name var =
    mapM (genPrimaryClause instanceOf [var] call exp) (ctrsForClass instanceOf)
        where
            call = AppE (VarE $ mkName $ name ++ "'") (VarE var)
            exp  = \s -> AppE (AppE (VarE $ mkName name) s) (VarE var) -- TODO
-- | `genModDefs mode name fname' generates the getter, the setter, and the
--   field selector for a field named `fname' in a state class named `name'
--   using routing mode `mode'.
genField :: StateDecl -> StateDecl -> ImplMode -> String -> String -> Q [Dec]
genField dec instanceOf mode name fname = do
    let
        bname   = "_" ++ fname                      -- the base name of the field
        gname   = "_get" ++ bname                   -- the name of the getter
        sname   = "_set" ++ bname                   -- the name of the setter
        lname   = lensName name ++ "_" ++ fname     -- the name of the lens for this field
    --gdcl <- genDataClause mode name [] (VarE $ mkName $ gname ++ "'")
    --gscl <- genStartClause mode name [] (AppE (VarE $ mkName gname))
    gcls <- genGetterClauses mode dec instanceOf gname
    let
        --gcls    = [gdcl,gscl]
        ext_g   = mkName gname
        int_g   = mkName $ gname ++ "'"
        getter  = FunD ext_g gcls
        getter' = genIntGetter (isBaseClass dec) mode lname int_g
    v    <- newName "v"
    --sdcl <- genDataClause mode name [v] (AppE (VarE $ mkName $ sname ++ "'") (VarE v))
    --sscl <- genStartClause mode name [v] (\s -> AppE (AppE (VarE $ mkName sname) s) (VarE v))
    scls <- genSetterClauses mode dec instanceOf sname v
    let
        --scls    = [sdcl,sscl]
        ext_s   = mkName sname
        int_s   = mkName $ sname ++ "'"
        setter  = FunD ext_s scls
        setter' = genIntSetter (isBaseClass dec) mode lname int_s
        field   = FunD (mkName fname) [Clause [] (NormalB $ foldl AppE (ConE $ mkName "MkField") [VarE $ mkName gname, VarE $ mkName $ gname ++ "'", VarE $ mkName sname, VarE $ mkName $ sname ++ "'" ]) []]
    return [getter,getter',setter,setter',field]

-- | `genFields dec mode' generates getters, setters, and field selectors
--   for the fields in `ds' which are part of a state class @dec. `mode'
--   determines how these calls will be routed.
genFields :: StateDecl -> StateDecl -> ImplMode -> Q [Dec]
genFields dec instanceOf mode =
    trace ("Generating fields for " ++ stateName dec ++ ":" ++ stateName instanceOf ++ " in mode " ++ show mode) $
    concat <$> mapM (genField dec instanceOf mode (stateName dec)) fs
    where
        -- the names of the fields in dec
        fs = map stateDataName (stateData dec)

{--------------------------------------------------------------------------
    Methods
--------------------------------------------------------------------------}

findClassMethodType :: [Dec] -> String -> Type
findClassMethodType [] m = error $ "Method not defined: " ++ m
findClassMethodType (SigD n t : ds) m
    | nameBase n == m = t
    | otherwise       = findClassMethodType ds m
findClassMethodType (_ : ds) m = findClassMethodType ds m

numArgsForMethod :: Dec -> String -> Int
numArgsForMethod (ClassD _ _ _ _ ds) n =
    countTypeArgs $ findClassMethodType ds n

genSelectorWrapper :: [Name] -> Exp -> Exp
genSelectorWrapper [] exp = exp
genSelectorWrapper ns exp = LamE (map VarP ns) exp

genInternalWrapper :: Name -> [Name] -> Exp
genInternalWrapper iname [] = VarE iname --AppE (VarE $ mkName "const") (VarE iname)
genInternalWrapper iname vs = foldl AppE (VarE iname) (map VarE vs) -- LamE [TupP $ map VarP vs] (AppE (VarE iname) (TupE $ map VarE vs))

genExternalWrapper :: Name -> [Name] -> Exp
genExternalWrapper ename [] = LamE [VarP obj] $ AppE (VarE ename) (VarE obj)
    where
        obj = mkName "obj"
genExternalWrapper ename vs = LamE [VarP obj] $ foldl AppE (AppE (VarE ename) (VarE obj)) (map VarE vs)
    where
        obj = mkName "obj"

genMethodClauses :: ImplMode -> StateDecl -> StateDecl -> Name -> Name -> [Name] -> Q [Clause]
genMethodClauses PrimaryInst decl instanceOf iname ename vs = mapM (genPrimaryClause instanceOf vs call exp) (ctrsForClass instanceOf)
    where
        call = foldl AppE (VarE iname) (map VarE vs)
        exp  = \s -> foldl AppE (AppE (VarE ename) s) (map VarE vs)
genMethodClauses IdentityInst decl instanceOf iname ename vs = do
    obj <- newName "obj"
    let
        call = LamE [VarP obj] $ foldl AppE (VarE ename) (VarE obj : map VarE vs)
    catMaybes <$> mapM (genIdentityClause decl instanceOf vs call) (ctrsForClass instanceOf)

genMethodClauses SecondaryInst decl instanceOf iname ename vs = mapM (genPrimaryClause instanceOf vs call exp) (ctrsForClass instanceOf)
    where
        call = foldl AppE (VarE iname) (map VarE vs)
        exp  = \s -> foldl AppE (AppE (VarE ename) s) (map VarE vs)

genMethod' :: ImplMode -> StateDecl -> StateDecl -> MethodTable -> String -> String -> Type -> Q [Dec]
genMethod' mode decl instanceOf tbl cn name typ = do
    -- if this method was declared by a parent, it belongs to
    -- a different type, so we don't implement it here
    if declByParent (mkName name) decl then return []
    else do
        let
            argc = countTypeArgs typ -- numArgsForMethod cls ("_icall_" ++ name)
            -- external call name
            ename    = mkName $ "_ecall_" ++ name
            -- internal call name
            iname    = mkName $ "_icall_" ++ name
        vs   <- replicateM argc (newName "v")
        eclauses <- genMethodClauses mode decl instanceOf iname ename vs
        let
            -- external
            external = FunD ename eclauses
            -- internal
            mname    = mkName $ "_" ++ (stateName instanceOf) ++ "_" ++ name
            iclauses = if isAbstract (mkName name) instanceOf
                       then [Clause [] (NormalB (AppE (VarE $ mkName "error") (VarE $ mkName "_msh_rt_invalid_call_abstract"))) []]
                       else if isImplemented (mkName name) tbl
                            then [Clause [] (NormalB (VarE mname)) []]
                            else [Clause [] (NormalB (lifted $ VarE iname)) []]
            internal = FunD iname iclauses
            -- method
            iwrapper = genInternalWrapper iname vs
            ewrapper = genExternalWrapper ename vs
            swrapper = genSelectorWrapper vs (foldl AppE (ConE $ mkName "MkMethod") [iwrapper, ewrapper])
            mclauses = [Clause [] (NormalB swrapper) []]
            method   = FunD (mkName $ name) mclauses
        trace (show ename ++ show mode) $ return [external, internal, method]

-- | `genMethod env cls mp cn d' generates a method for based on `d'.
genMethod :: ImplMode -> StateDecl -> StateDecl -> MethodTable -> String -> (String, Dec) -> Q [Dec]
genMethod mode decl instanceOf tbl cn (name, SigD _ ty)          =
    genMethod' mode decl instanceOf tbl cn name ty
--genMethodDef env tbl cls mp cn (FunD name _)          = genMethodDef' env cls mp cn (nameBase name)
--genMethodDef env tbl cls mp cn (ValD (VarP name) _ _) = genMethodDef' env cls mp cn (nameBase name)
genMethod _    _   _    _   _ _                         = return []

-- | Generates methods.
genMethods :: ImplMode -> StateDecl -> StateDecl -> MethodTable -> String -> Q [Dec]
genMethods mode decl instanceOf tbl cn = trace ("Generating methods: " ++ show (M.toList $ methodSigs tbl)) $
    concat <$> mapM (genMethod mode decl instanceOf tbl cn) (M.toList $ methodSigs tbl)
