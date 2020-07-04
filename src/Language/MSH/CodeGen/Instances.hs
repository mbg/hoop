{-# LANGUAGE TemplateHaskell #-}

module Language.MSH.CodeGen.Instances where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)

import Data.Char (toLower)
import qualified Data.Map as M 

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.StateEnv
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Inheritance
import Language.MSH.CodeGen.SharedInstance (genRunStateT, genInvoke)
import Language.MSH.CodeGen.ObjectInstance (genObjectInstance)
import Language.MSH.CodeGen.PrimaryInstance (genPrimaryInstance, genIdentityInstance, genParentalInstance)

{--------------------------------------------------------------------------
    Type class instances
--------------------------------------------------------------------------}



-- | Enumerates different member generation modes.
data MemberGenMode = Primary    -- ^ Generated members will correspond to their implementations
                   | Lift       -- ^ Generated members will forward calls to the parent, unless overriden
                   | Invoke     -- ^ Generates members will call the `_invoke' method to construct a monad stack 

genDataClause :: MemberGenMode -> String -> [Name] -> Exp -> Q Clause 
-- [Primary] (Data d) x0..xn = do { (r,d') <- runStateT expr d; return (r, Data d') }
genDataClause Primary name vars expr = do
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
-- [Invoke]  (Data d) x0..xn = error ""
genDataClause Invoke name vars expr = do 
    d  <- newName "d"
    r  <- newName "r"
    d' <- newName "d'"
    let
        ctr  = mkName $ name ++ "Data"
        pat  = ConP ctr [VarP d] : map VarP vars
        body = AppE (VarE $ mkName "error") (VarE $ mkName "_msh_rt_invalid_call_state")
    return $ Clause pat (NormalB body) []

genStartClause :: MemberGenMode -> String -> [Name] -> (Exp -> Exp) -> Q Clause 
-- (Start d s) x0...xn = do { ((r,s'),d') <- runStateT (expr s); return (r, Start d' s') }
genStartClause Primary name vars expr = do
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
genStartClause Invoke name vars expr = do 
    d  <- newName "d"
    s  <- newName "s"
    let
        ctr  = mkName $ name ++ "Start"
        pat  = ConP ctr [VarP d, VarP s] : map VarP vars
        body = AppE (VarE $ mkName "error") (VarE $ mkName "_msh_rt_invalid_call_state")
    return $ Clause pat (NormalB body) []

genMiddleClause :: MemberGenMode -> StateDecl -> [Name] -> Exp -> Q Clause 
{-genMiddleClause Primary (StateDecl { stateName = name }) vars expr = do 
    d  <- newName "d"
    s  <- newName "s"
    r  <- newName "r"
    d' <- newName "d'"
    s' <- newName "s'"
    p  <- newName "p"
    let
        ctr  = mkName $ name ++ "Middle"
        pat  = ConP ctr [VarP p, VarP d, VarP s] : map VarP vars
        bpat = TupP [TupP [VarP r, VarP s'], VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p, VarE d', VarE s']])
        body = DoE [BindS bpat (genRunStateT (expr (VarE s)) (VarE d)), NoBindS ret]
    return $ Clause pat (NormalB body) []-}
genMiddleClause Invoke (StateDecl { stateName = name, stateParent = Just parent }) vars expr = do 
    p  <- newName "p"
    d  <- newName "d"
    s  <- newName "s"
    r  <- newName "r"
    o  <- newName "o"
    p' <- newName "p'"
    obj <- newName "obj"
    let
        ctr  = mkName $ name ++ "Middle"
        pat  = (AsP obj $ ConP ctr [VarP p, VarP d, VarP s]) : map VarP vars
        dn   = mkName $ "_" ++ name ++ "_data"
        sn   = mkName $ "_" ++ name ++ "_sub"
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p', AppE (VarE dn) (VarE o), AppE (VarE sn) (VarE o)]])
        bpat = TupP [VarP r, VarP p', VarP o]
        body = DoE [BindS bpat (genInvoke (stateName parent) (VarE obj) expr (VarE s)), NoBindS ret]
    return $ Clause pat (NormalB body) []

genEndClause :: MemberGenMode -> String -> [Name] -> Exp -> Q Clause 
genEndClause Primary name vars expr = do
    d  <- newName "d"
    r  <- newName "r"
    d' <- newName "d'"
    p  <- newName "p"
    let
        ctr  = mkName $ name ++ "End"
        pat  = ConP ctr [VarP p, VarP d] : map VarP vars
        bpat = TupP [VarP r, VarP d']
        ret  = AppE (VarE $ mkName "return") (TupE [VarE r, foldl AppE (ConE ctr) [VarE p, VarE d']])
        body = DoE [BindS bpat (genRunStateT expr (VarE d)), NoBindS ret]
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

{--------------------------------------------------------------------------
    Fields
--------------------------------------------------------------------------}

{-lensName :: String -> String 
lensName (x:xs) = toLower x : xs

genGetterBody :: MemberGenMode -> String -> Name -> Exp 
genGetterBody Primary lens self = AppE (VarE $ mkName "use") (VarE $ mkName lens)
genGetterBody Lift    lens self = AppE (VarE $ mkName "lift") (VarE self)
genGetterBody Invoke  lens self = AppE (VarE $ mkName "error") (LitE $ 
    StringL "Invalid call: trying to construct monad stack in an internal getter call.")
 
-- | `genModDefs mode name fname' generates the getter, the setter, and the
--   field selector for a field named `fname' in a state class named `name'
--   using routing mode `mode'.
genModDefs :: MemberGenMode -> String -> String -> Q [Dec]
genModDefs mode name fname = do
    let
        bname   = "_" ++ fname                      -- the base name of the field
        gname   = "_get" ++ bname                   -- the name of the getter
        sname   = "_set" ++ bname                   -- the name of the setter
        lname   = lensName name ++ "_" ++ fname     -- the name of the lens for this field
    gdcl <- genDataClause mode name [] (VarE $ mkName $ gname ++ "'")
    gscl <- genStartClause mode name [] (AppE (VarE $ mkName gname))
    let
        gcls    = [gdcl,gscl]
        ext_g   = mkName gname 
        int_g   = mkName $ gname ++ "'"
        getter  = FunD ext_g gcls
        getter' = FunD int_g [Clause [] (NormalB $ genGetterBody mode lname int_g) []]
    v    <- newName "v"
    sdcl <- genDataClause mode name [v] (AppE (VarE $ mkName $ sname ++ "'") (VarE v))
    sscl <- genStartClause mode name [v] (\s -> AppE (AppE (VarE $ mkName sname) s) (VarE v))
    let
        scls    = [sdcl,sscl]
        ext_s   = mkName sname 
        int_s   = mkName $ sname ++ "'"
        setter  = FunD ext_s scls
        setter' = FunD int_s [Clause [] (NormalB (AppE (VarE $ mkName "assign") (VarE $ mkName lname))) []]
        field   = FunD (mkName fname) [Clause [] (NormalB $ appEs (ConE $ mkName "MkField") [VarE $ mkName gname, VarE $ mkName $ gname ++ "'", VarE $ mkName sname, VarE $ mkName $ sname ++ "'" ]) []]
    return [getter,getter',setter,setter',field]

-- | `genModsDefs mode name ds' generates getters, setters, and field selectors
--   for the fields in `ds' which are part of a state class named `name'. `mode'
--   determines how these calls will be routed.
genModsDefs :: MemberGenMode -> String -> [StateMemberDecl] -> Q [Dec]
genModsDefs mode name ds = 
    concat <$> mapM (genModDefs mode name) (map stateDataName ds)

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
        
genMethodDef' :: MemberGenMode -> StateEnv -> StateDecl -> MethodTable -> Dec -> Maybe String -> String -> String -> Q [Dec]
genMethodDef' mode env decl tbl cls mp cn name = do
    ov <- isInherited env mp (mkName name)
    if ov then return []
    else do
        let
            argc = numArgsForMethod cls ("_icall_" ++ name)
            -- external call name
            ename    = mkName $ "_ecall_" ++ name
            -- internal call name
            iname    = mkName $ "_icall_" ++ name
        vs   <- replicateM argc (newName "v")
        -- TODO: these should be generated per call?
        edcl <- genDataClause mode cn vs (appEs (VarE iname) (map VarE vs))
        escl <- genStartClause mode cn vs (\s -> appEs (AppE (VarE ename) s) (map VarE vs)) 
        --emcl <- genMiddleClause mode cn vs (VarE iname) 
        let
            -- external
            eclauses = [edcl, escl] -- TODO: this should match the avail. constructors
            external = FunD ename eclauses
            -- internal
            mname    = mkName $ "_" ++ cn ++ "_" ++ name
            iclauses = if isAbstract (mkName name) tbl 
                       then [Clause [] (NormalB (AppE (VarE $ mkName "error") (VarE $ mkName "_msh_rt_invalid_call_abstract"))) []]
                       else [Clause [] (NormalB (VarE mname)) []]
            internal = FunD iname iclauses
            -- method
            iwrapper = genInternalWrapper iname vs
            ewrapper = genExternalWrapper ename vs
            swrapper = genSelectorWrapper vs (appEs (ConE $ mkName "MkMethod") [iwrapper, ewrapper])
            mclauses = [Clause [] (NormalB swrapper) []]
            method   = FunD (mkName $ name) mclauses 
        return [external, internal, method]

-- | `genMethodDef env cls mp cn d' generates a method for based on `d'.
genMethodDef :: MemberGenMode -> StateEnv -> StateDecl -> MethodTable -> Dec -> Maybe String -> String -> Dec -> Q [Dec]
genMethodDef mode env decl tbl cls mp cn (SigD name _)          = genMethodDef' mode env decl tbl cls mp cn (nameBase name)
--genMethodDef env tbl cls mp cn (FunD name _)          = genMethodDef' env cls mp cn (nameBase name)
--genMethodDef env tbl cls mp cn (ValD (VarP name) _ _) = genMethodDef' env cls mp cn (nameBase name)
genMethodDef _    _   _    _   _   _  _  _                      = return []

genMethodsDefs :: MemberGenMode -> StateEnv -> Dec -> StateDecl -> MethodTable -> Maybe String -> String -> Q [Dec]
genMethodsDefs mode env cls decl tbl mp cn = 
    concat <$> mapM (genMethodDef mode env decl tbl cls mp cn) (M.elems $ methodSigs tbl)

getBaseMonad :: Maybe String -> Type 
getBaseMonad Nothing  = ConT $ mkName "Identity"
getBaseMonad (Just p) = renameParent (\n -> n ++ "M") $ parseType p-}

{-genPrimaryInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec 
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
        ty  = appN (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) vars
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
    invk <- genInvokeDef name
    mods <- genModsDefs Primary name ds
    ms   <- genMethodsDefs Primary env cls decl methods mp name
    return $ InstanceD cxt ty ([fam,invk] ++ mods ++ ms)-}

{-genIdentityInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec 
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
        ty  = appN (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) vars
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
    invk <- genInvokeDef name
    mods <- genModsDefs Invoke name ds
    ms   <- genMethodsDefs Invoke env cls decl methods mp name
    return $ InstanceD cxt ty ([fam,invk] ++ mods ++ ms)-}

-- TODO: do this recursively
-- TODO: method bodies
{-genParentalInstance :: StateDecl -> StateDecl -> Q [Dec] 
genParentalInstance sub parent = do
    let
        cxt = []
        cn  = mkName $ (stateName parent) ++ "Like"
        on  = mkName (stateName sub)
        sn  = mkName $ (stateName sub) ++ "State"
        bt  = getBaseMonad (stateParentN sub) 
        -- TODO: not sure if the parameters should be from the parent or inferred from the parent type?
        ty  = foldl AppT (ConT cn) ([ConT on, ConT sn, bt] ++ map (VarT . mkName) (stateParams parent))
        idty = foldl AppT (ConT cn) ([ConT on, ConT sn, ConT $ mkName "Identity"] ++ map (VarT . mkName) (stateParams parent))
    rs <- case stateParent parent of 
        Nothing  -> return []
        (Just p) -> genParentalInstance sub p 
    return $ [ InstanceD cxt ty []
           , InstanceD cxt idty []] ++ rs-}

-- | Generates instances of the parental type classes.
genParentalInstances :: StateEnv -> StateDecl -> Q [Dec]
genParentalInstances _ StateDecl{ stateParent = Nothing } =
    return []
genParentalInstances env s@StateDecl{ stateParent = Just parent } = do
    genParentalInstance s parent

-- | Generates type class instances for a state declaration
--   For a base class, there will be one instance of the corresponding type class
--   For sub-classes, there will be two instances of the corresponding type class, 
--   as well as instances of all parent classes
genStateInstances :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q [Dec]
genStateInstances env cls decs s = do
    -- generate the instance for the `Object' class -- one per state class
    obj <- genObjectInstance s
    -- generate the primary instance (CLike C CData PMonad)
    p   <- genPrimaryInstance env cls decs s
    -- generate the parental instances (PLike C CData AMonad)
    ii  <- if isBaseClass s 
           then return []
           else do
            -- generate the identity instance (CLike C CData Identity)
            i  <- genIdentityInstance env cls decs s
            -- generate the parental instances (PLike C CData PPMonad & PLike C CData Identity)
            ps <- genParentalInstances env s
            return $ i : ps
    return $ [p] ++ ii ++ obj
