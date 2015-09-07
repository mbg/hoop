module Language.MSH.CodeGen.Instances where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Data.Char (toLower)
import qualified Data.Map as M 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Inheritance

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

genSelectorWrapper :: [Name] -> Exp -> Exp
genSelectorWrapper [] exp = exp
genSelectorWrapper ns exp = LamE (map VarP ns) exp

genInternalWrapper :: Name -> [Name] -> Exp 
genInternalWrapper iname [] = VarE iname --AppE (VarE $ mkName "const") (VarE iname)
genInternalWrapper iname vs = appEs (VarE iname) (map VarE vs) -- LamE [TupP $ map VarP vs] (AppE (VarE iname) (TupE $ map VarE vs))

genExternalWrapper :: Name -> [Name] -> Exp
genExternalWrapper ename [] = LamE [VarP obj] $ AppE (VarE ename) (VarE obj)
    where
        obj = mkName "obj"
genExternalWrapper ename vs = LamE [VarP obj] $ appEs (AppE (VarE ename) (VarE obj)) (map VarE vs)
    where
        obj = mkName "obj"
        
genMethodDef' :: StateEnv -> Dec -> Maybe String -> String -> String -> Q [Dec]
genMethodDef' env cls mp cn name = do
    ov <- isInherited env mp (mkName name)
    if ov then return []
    else do
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
            swrapper = genSelectorWrapper vs (appEs (ConE $ mkName "MkMethod") [iwrapper, ewrapper])
            mclauses = [Clause [] (NormalB swrapper) []]
            method   = FunD (mkName $ name) mclauses 
        return [external, internal, method]

genMethodDef :: StateEnv -> Dec -> Maybe String -> String -> Dec -> Q [Dec]
genMethodDef env cls mp cn (FunD name _)          = genMethodDef' env cls mp cn (nameBase name)
genMethodDef env cls mp cn (ValD (VarP name) _ _) = genMethodDef' env cls mp cn (nameBase name)
genMethodDef _   _   _  _  _                      = return []

genMethodsDefs :: StateEnv -> Dec -> [Dec] -> Maybe String -> String -> Q [Dec]
genMethodsDefs env cls decs mp cn = concat <$> mapM (genMethodDef env cls mp cn) decs

getBaseMonad :: Maybe String -> Type 
getBaseMonad Nothing  = ConT $ mkName "Identity"
getBaseMonad (Just p) = renameParent (\n -> n ++ "M") $ parseType p

genPrimaryInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec 
genPrimaryInstance env cls decs (StateDecl {
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
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
    invk <- genInvokeDef name
    mods <- genModsDefs name ds
    ms   <- genMethodsDefs env cls decs mp name
    return $ InstanceD cxt ty ([fam,invk] ++ mods ++ ms)

genObjectTypeInsts :: Type -> Type -> Q [Dec]
genObjectTypeInsts obj st = do
    m <- VarT `fmap` newName "m"
    s <- VarT `fmap` newName "st"
    r <- VarT `fmap` newName "r"
    return [ TySynInstD (mkName "QueryObject") $ TySynEqn [obj] obj
           , TySynInstD (mkName "QueryMonad")  $ TySynEqn [obj, m] m
           , TySynInstD (mkName "QueryResult") $ TySynEqn [obj, s, m, r] 
                (foldl AppT (ConT $ mkName "RunnableQuery") [ ConT (mkName "ExtCall")
                                                            , obj, st, m, r ])]

-- | `genObjectInstance decl' generates an instance of `Object' 
--   for the state declaration `decl'. Note: only one such instance
--   is needed per state decl.
genObjectInstance :: StateDecl -> Q [Dec] 
genObjectInstance (StateDecl { stateName = name, stateParams = bars{-, stateParent = (Just ps)-} }) = do
    let
        obj = appN (ConT $ mkName name) bars
        st  = appN (ConT $ mkName $ name ++ "State") bars
    -- The name of the arbitrary monad this instance is for.
    m <- newName "m"
    let
        --p = parseType ps
        --(Name pn _) = parentName p
        --pcname      = mkName $ occString pn ++ "M"
        --vars        = parentArgs p
        cxt = [AppT (ConT $ mkName "Monad") (VarT m)]
        --m = (appN' (ConT pcname) vars)
        --m = ConT $ mkName "Identity"
        ty  = AppT (AppT (AppT (ConT $ mkName "Object") obj) st) (VarT m)
        ost = TySynInstD (mkName "ObjSt") $ TySynEqn [obj] st
        cl1 = Clause [VarP $ mkName "obj", ConP (mkName "MkMethod") [WildP, VarP $ mkName "e"]] (NormalB $ AppE (ConE $ mkName "MkExtCall") (AppE (VarE $ mkName "e") (VarE $ mkName "obj"))) []
        eqn = FunD (mkName ".!") [cl1]
        ds  = [ost, eqn]
    fams <- genObjectTypeInsts obj st
    return $ InstanceD cxt ty ds : fams
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

genParentalInstanceFromInfo :: StateDecl -> Info -> Q Dec 
genParentalInstanceFromInfo sub (ClassI (ClassD _ cn vars _ _) _) = do
    ps <- replicateM (length vars - 3) (newName "p")
    let
        cxt = []
        on  = mkName (stateName sub)
        sn  = mkName $ (stateName sub) ++ "State"
        bt  = getBaseMonad (stateParent sub)
        ty  = appN (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) (map nameBase ps) -- TODO: not sure if this should be parent or inferred from the parent type?
    return $ InstanceD cxt ty []

-- | Generates instances of the parental type classes.
genParentalInstances :: StateEnv -> StateDecl -> Q [Dec]
genParentalInstances _ (StateDecl _ _ _ Nothing _ _) = do
    return []
genParentalInstances env s@(StateDecl m name vars (Just p) ds decls) = do
    let
        pt = parseType p
        pn = parentName pt
    case M.lookup (nameBase pn) env of
        Nothing  -> do
            mn <- lookupTypeName $ nameBase pn ++ "Like"
            case mn of 
                Nothing -> do
                    fail $ "`" ++ (nameBase pn) ++ "' is neither a state class in the current quote nor is it in scope."
                (Just n) -> do
                    info <- reify n
                    i <- genParentalInstanceFromInfo s info
                    return [i]
        (Just p) -> do
            i <- genParentalInstance s p
            return [i]  

-- | Generates type class instances for a state declaration
--   For a base class, there will be one instance of the corresponding type class
--   For sub-classes, there will be two instances of the corresponding type class, 
--   as well as instances of all parent classes
genStateInstances :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q [Dec]
genStateInstances env cls decs s@(StateDecl m name vars p ds decls) = do
    obj <- genObjectInstance s
    p   <- genPrimaryInstance env cls decs s
    ps  <- genParentalInstances env s
    return $ (p : ps) ++ obj
