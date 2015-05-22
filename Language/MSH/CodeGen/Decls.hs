{-# LANGUAGE TemplateHaskell #-}

module Language.MSH.CodeGen.Decls (
    genStateDecls
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
--import Control.Monad.State

import Data.Char (toLower)
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Lens.TH
import Control.Lens.Internal.FieldTH

-- needed to parse Haskell syntax and to convert it into TH syntax
import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.Parser as Exts
import Language.Haskell.Exts.Extension
import Language.Haskell.Meta.Syntax.Translate (toType, toDecs, toExp)

import Language.MSH.StateDecl
import Language.MSH.Parsers
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Data
import Language.MSH.CodeGen.Object
import Language.MSH.CodeGen.Class
import Language.MSH.CodeGen.Methods
import Language.MSH.CodeGen.Constructors
import Language.MSH.CodeGen.MiscInstances

type StateEnv = M.Map String StateDecl

genStateType :: [TyVarBndr] -> StateDecl -> Q Dec 
genStateType tyvars (StateDecl m name vars mp ds decls) = do
    let
        -- unlike in the paper, the type synonym isn't just the name of the class
        tname = mkName $ name ++ "M"
        stype = appN (ConT (mkName $ name ++ "State")) vars
    case mp of
        Nothing  -> return $ TySynD tname tyvars (AppT (ConT (mkName "State")) stype)
        (Just p) -> do
            let
                ptype = parseType p 
                -- we want the monad, not the object
                fixpt t@(ConT _) = renameT (\n -> n ++ "M") t
                fixpt (AppT f a) = AppT (fixpt f) a 
            return $ TySynD tname tyvars (AppT (AppT (ConT (mkName "StateT")) stype) (fixpt ptype))


{-
    Type class instances
-}

genRunStateT :: Exp -> Exp -> Exp 
genRunStateT f d = AppE (AppE (VarE $ mkName "runStateT") f) d

genDataClause :: String -> [Name] -> Exp -> Q Clause 
genDataClause name vars expr = do
    d  <- newName "d"
    r  <- newName "r"
    d' <- newName "d'"
    let
        ctr  = mkName $ name ++ "Data"
        pat  = ConP ctr [VarP d] : map VarP vars
        bpat = TupP [VarP r, VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, AppE (ConE ctr) (VarE d')])
        body = DoE [BindS bpat (genRunStateT expr (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []

genStartClause :: String -> [Name] -> (Exp -> Exp) -> Q Clause 
genStartClause name vars expr = do
    d  <- newName "d"
    s  <- newName "s"
    r  <- newName "r"
    d' <- newName "d'"
    s' <- newName "s'"
    let
        ctr  = mkName $ name ++ "Start"
        pat  = ConP ctr [VarP d, VarP s] : map VarP vars
        bpat = TupP [TupP [VarP r, VarP s'], VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, AppE (AppE (ConE ctr) (VarE d')) (VarE s')])
        body = DoE [BindS bpat (genRunStateT (expr (VarE s)) (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []

findClassMethodType :: [Dec] -> String -> Type
findClassMethodType [] m = error $ "Method not defined: " ++ m
findClassMethodType (SigD n t : ds) m
    | nameBase n == m = t
    | otherwise       = findClassMethodType ds m 
findClassMethodType (_ : ds) m = findClassMethodType ds m

countTypeArgs :: Type -> Int
countTypeArgs (ForallT _ _ t)          = countTypeArgs t
countTypeArgs (AppT (AppT ArrowT _) a) = 1 + countTypeArgs a
countTypeArgs _                        = 0

numArgsForMethod :: Dec -> String -> Int 
numArgsForMethod (ClassD _ _ _ _ ds) n = 
    countTypeArgs $ findClassMethodType ds n

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

lensName :: String -> String 
lensName (x:xs) = toLower x : xs

genModDefs :: String -> String -> Q [Dec]
genModDefs name fname = do
    let
        bname   = "_" ++ fname 
        gname   = "_get" ++ bname
        sname   = "_set" ++ bname
        lname   = lensName name ++ "_" ++ fname
    gdcl <- genDataClause name [] (VarE $ mkName $ gname ++ "'")
    gscl <- genStartClause name [] (AppE (VarE $ mkName gname))
    let
        gcls    = [gdcl,gscl]
        getter  = FunD (mkName gname) gcls
        getter' = FunD (mkName $ gname ++ "'") [Clause [] (NormalB (AppE (VarE $ mkName "use") (VarE $ mkName lname))) []]
    v    <- newName "v"
    sdcl <- genDataClause name [v] (AppE (VarE $ mkName $ sname ++ "'") (VarE v))
    sscl <- genStartClause name [v] (\s -> AppE (AppE (VarE $ mkName sname) s) (VarE v))
    let
        scls    = [sdcl,sscl]
        setter  = FunD (mkName sname) scls
        setter' = FunD (mkName $ sname ++ "'") [Clause [] (NormalB (AppE (VarE $ mkName "assign") (VarE $ mkName lname))) []]
        field   = FunD (mkName fname) [Clause [] (NormalB $ appEs (ConE $ mkName "MkField") [VarE $ mkName gname, VarE $ mkName $ gname ++ "'", VarE $ mkName sname, VarE $ mkName $ sname ++ "'" ]) []]
    return [getter,getter',setter,setter',field]

genModsDefs :: String -> [StateMemberDecl] -> Q [Dec]
genModsDefs name ds = concat <$> mapM (genModDefs name) (map stateDataName ds)

genInternalWrapper :: Name -> [Name] -> Exp 
genInternalWrapper iname [] = AppE (VarE $ mkName "const") (VarE iname)
genInternalWrapper iname vs = LamE [TupP $ map VarP vs] (AppE (VarE iname) (TupE $ map VarE vs))

genExternalWrapper :: Name -> [Name] -> Exp
genExternalWrapper ename [] = LamE [VarP obj] $ AppE (VarE $ mkName "const") (AppE (VarE ename) (VarE obj))
    where
        obj = mkName "obj"
genExternalWrapper ename vs = LamE [VarP obj, TupP $ map VarP vs] $ appEs (VarE ename) [VarE obj, TupE $ map VarE vs]
    where
        obj = mkName "obj"
        
genMethodDef' :: Dec -> String -> String -> Q [Dec]
genMethodDef' cls cn name = do
    let
        argc = numArgsForMethod cls ("_icall_" ++ name)
        -- external
        ename    = mkName $ "_ecall_" ++ name
        -- internal
        iname    = mkName $ "_icall_" ++ name
    vs   <- replicateM argc (newName "v")
    edcl <- genDataClause cn vs (appEs (VarE iname) (map VarE vs))
    escl <- genStartClause cn vs (\s -> appEs (AppE (VarE ename) s) (map VarE vs)) 
    let
        -- external
        eclauses = [edcl, escl]
        external = FunD ename eclauses
        -- internal
        mname    = mkName $ "_" ++ cn ++ "_" ++ name
        iclauses = [Clause [] (NormalB (VarE mname)) []]
        internal = FunD iname iclauses
        -- method
        iwrapper = genInternalWrapper iname vs
        ewrapper = genExternalWrapper ename vs
        mclauses = [Clause [] (NormalB (appEs (ConE $ mkName "MkMethod") [iwrapper, ewrapper])) []]
        method   = FunD (mkName $ name) mclauses 
    return [external, internal, method]

genMethodDef :: Dec -> String -> Dec -> Q [Dec]
genMethodDef cls cn (FunD name _)          = genMethodDef' cls cn (nameBase name)
genMethodDef cls cn (ValD (VarP name) _ _) = genMethodDef' cls cn (nameBase name)
genMethodDef _   _  _                      = return []

genMethodsDefs :: Dec -> [Dec] -> String -> Q [Dec]
genMethodsDefs cls decs cn = concat <$> mapM (genMethodDef cls cn) decs

getBaseMonad :: Maybe String -> Type 
getBaseMonad Nothing  = ConT $ mkName "Identity"
getBaseMonad (Just p) = renameParent (\n -> n ++ "M") $ parseType p

genPrimaryInstance :: Dec -> [Dec] -> StateDecl -> Q Dec 
genPrimaryInstance cls decs (StateDecl {
    stateName   = name, 
    stateParams = vars,
    stateData   = ds,
    stateParent = mp
}) = do
    let
        cxt = []
        cn  = mkName $ name ++ "Like"
        on  = mkName name
        sn  = mkName $ name ++ "State"
        bt  = getBaseMonad mp 
        ty  = appN (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) vars
    invk <- genInvokeDef name
    mods <- genModsDefs name ds
    ms   <- genMethodsDefs cls decs name
    return $ InstanceD cxt ty ([invk] ++ mods ++ ms)

genObjectInstance :: StateDecl -> Q [Dec] 
genObjectInstance (StateDecl { stateName = name, stateParams = bars, stateParent = (Just ps) }) = do
    m <- newName "m"
    let
        p = parseType ps
        (Name pn _) = parentName p
        pcname      = mkName $ occString pn ++ "M"
        vars        = parentArgs p
        cxt = [{-ClassP (mkName "Monad") [VarT m]-}]
        ty  = AppT (AppT (ConT $ mkName "Object") (appN (ConT $ mkName name) bars)) (appN' (ConT pcname) vars)
    return [InstanceD cxt ty []]
genObjectInstance _ = return []

genParentalInstance :: StateDecl -> StateDecl -> Q Dec 
genParentalInstance sub parent = do
    let
        cxt = []
        cn  = mkName $ (stateName parent) ++ "Like"
        on  = mkName (stateName sub)
        sn  = mkName $ (stateName sub) ++ "State"
        bt  = getBaseMonad (stateParent sub)
        ty  = appN (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) (stateParams parent) -- TODO: not sure if this should be parent or inferred from the parent type?
    return $ InstanceD cxt ty []

genParentalInstances :: StateEnv -> StateDecl -> Q [Dec]
genParentalInstances _ (StateDecl _ _ _ Nothing _ _) = do
    return []
genParentalInstances env s@(StateDecl m name vars (Just p) ds decls) = do
    let
        pt = parseType p
        pn = parentName pt
    case M.lookup (nameBase pn) env of
        Nothing  -> error $ "`" ++ (nameBase pn) ++ "' is not a state class in the current quote."
        (Just p) -> do
            i <- genParentalInstance s p
            return [i]   

genIdentityInstance :: Q Dec 
genIdentityInstance = do
    let
        ty = tuple []
    return $ InstanceD [] ty []

-- | Generates type class instances for a state declaration
--   For a base class, there will be one instance of the corresponding type class
--   For sub-classes, there will be two instances of the corresponding type class, 
--   as well as instances of all parent classes
genStateInstances :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q [Dec]
genStateInstances env cls decs s@(StateDecl m name vars p ds decls) = do
    -- for both, there is an instance of the c
    obj <- genObjectInstance s
    p  <- genPrimaryInstance cls decs s
    ps <- genParentalInstances env s
    return $ (p : ps) ++ obj


{-
    External interface
-}

-- | Appends "_lens" to the lens names
lensLookup :: Name -> [Name] -> Name -> [DefName]
lensLookup _ fs field = [TopName $ mkName $ nameBase field ++ "_lens"] 

stateLensRules :: LensRules
stateLensRules = lensRules -- { _fieldToDef = lensLookup }

-- | Generates top-level declarations for a state declaration
genStateDecl :: StateEnv -> StateDecl -> Q [Dec]
genStateDecl env s@(StateDecl m name vars p ds decls) = do
    let
        tyvars   = map (PlainTV . mkName) vars
        decs     = parseDecs decls
    d  <- genStateData tyvars s
    ls <- makeFieldOpticsForDec stateLensRules d
    t  <- genStateType tyvars s
    o  <- genStateObject tyvars s
    c  <- genStateClass tyvars decs s
    is <- genStateInstances env c decs s
    cs <- genConstructors s
    misc <- genMiscInstances s o
    ms <- genMethods name vars decs
    return $ [d,t,o,c] ++ is ++ ls ++ cs ++ ms ++ misc

genStateDecls :: StateEnv -> Q [Dec]
genStateDecls env = do
    dss <- mapM (genStateDecl env) (M.elems env)
    return $ concat dss