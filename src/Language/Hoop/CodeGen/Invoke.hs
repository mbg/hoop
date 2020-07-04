--------------------------------------------------------------------------------
-- Monadic State Hierarchies                                                  --
-- Copyright 2013-2019 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Language.Hoop.CodeGen.Invoke (genInvoke) where 

--------------------------------------------------------------------------------

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.StateDecl
import Language.Hoop.CodeGen.Shared
import Language.Hoop.CodeGen.SharedInstance (genRunStateT)

--------------------------------------------------------------------------------

-- | Generates the typing for the `invoke' function.
genInvokeDecl :: [TyVarBndr] -> StateDecl -> Q Dec
genInvokeDecl tyvars StateDecl{..} = do
    -- initialise some fresh type variables
    o' <- newName "o'"
    d' <- newName "d'"
    r  <- newName "r"
    let
        objName = mkName stateName
        identity = mkName "Identity"
        objTy = appN (ConT objName) stateParams
        -- the name of the invoke function (e.g. _Expr_invoke)
        name  = mkName $ "_" ++ stateName ++ "_invoke"
        -- the name of the corresponding type class (e.g. ExprLike)
        cname = mkName $ stateName ++ "Like"
        -- the name of the corresponding monad (TODO: apply tyvars )
        base  = appN (ConT (mkName $ stateName ++ "M")) (stateParams)
        -- type class constraints (the context) on the type of the invoke function
        ctx   = [ foldl AppT (ConT cname) ([ConT objName, ConT $ mkName $ stateName ++ "State", ConT identity] ++ [VarT $ mkName n | n <- stateParams])
                , foldl AppT (ConT cname) ([VarT o', VarT d', base] ++ [VarT $ mkName n | n <- stateParams])
                ]
        -- ovs   = appN (VarT o) tyvars
        ovs'  = appN (VarT o') stateParams
        sigma = ovs' `arr` (ovs' `arr` AppT base (tuple [VarT r, ovs'])) `arr` 
                objTy `arr` AppT (ConT identity) (tuple [VarT r, objTy, ovs'])
        -- the type of the invoke function
        ty    = ForallT ([PlainTV o', PlainTV d', PlainTV r] ++ [PlainTV (mkName v) | v <- stateParams]) ctx sigma
    return $ SigD name ty

-- | Generates the implementation of the `_C_invoke' function.
--   The purpose of the `_C_invoke' functions is to allow a sub-class to
--   pass an arbitrary method to the super-class. It works as follows:
--
genInvokeDef :: [TyVarBndr] -> StateDecl -> Q Dec
genInvokeDef tyvars decl = case stateParent decl of 
    Nothing -> do
        s <- newName "s"
        f <- newName "f"
        o <- newName "o"
        r <- newName "r"
        d' <- newName "d'"
        s' <- newName "s'"
        let
            name = mkName $ "_" ++ stateName decl ++ "_invoke"
            fn   = mkName $ "_" ++ stateName decl ++ "_data"
            ps   = [VarP s, VarP f, VarP o]
            runs = BindS (TupP [TupP [VarP r, VarP s'], VarP d']) (genRunStateT (AppE (VarE f) (VarE s)) (AppE (VarE $ mkName "extractData") (VarE o)))
            rets = AppE (VarE $ mkName "return") (TupE [VarE r, RecUpdE (VarE o) [(fn,VarE d')], VarE s'])
            body = NormalB $ DoE [runs, NoBindS rets]
        return $ FunD name [Clause ps body []]
    Just pDecl -> do
        subObj <- newName "subObj"
        fun <- newName "fun"
        obj <- newName "obj"
        p <- newName "p"
        d <- newName "d"
        r <- newName "r"
        subObj' <- newName "subObj'"
        p' <- newName "p'"
        obj' <- newName "obj'"
        r0 <- newName "r0"
        subObj0 <- newName "subObj0"
        d0 <- newName "d0"
        let
            name = mkName $ "_" ++ stateName decl ++ "_invoke"
            pname = mkName $ "_" ++ stateName pDecl ++ "_invoke"
            endCtr = mkName $ stateName decl ++ "End"
            ps = [VarP subObj, VarP fun, AsP obj (ConP endCtr [VarP p, VarP d])]
            ed = AppE (VarE $ mkName "extractData") (VarE obj')
            innerRun = BindS (TupP [TupP [VarP r0, VarP subObj0], VarP d0]) (genRunStateT (AppE (VarE fun) (VarE subObj)) (VarE d))
            innerRet = appEs (VarE $ mkName "return") [TupE [TupE [VarE r0, VarE subObj0], appEs (ConE endCtr) [VarE p, VarE d0]]]
            wrapper = LamE [WildP] $ DoE [innerRun, NoBindS innerRet]
            subInvoke = appEs (VarE $ pname) [VarE obj, wrapper, VarE p]
            runs = BindS (TupP [TupP [VarP r, VarP subObj'], VarP p', VarP obj']) subInvoke
            rets = AppE (VarE $ mkName "return") (TupE [VarE r, appEs (ConE endCtr) [VarE p', ed], VarE subObj'])
            body = NormalB $ DoE [runs, NoBindS rets]
        return $ FunD name [Clause ps body []]

-- | `genInvoke` @decl@ generates the code for the @_C_invoke@ function of
-- a state class @decl@.
genInvoke :: [TyVarBndr] -> StateDecl -> Q [Dec]
genInvoke tyvars decl = do 
    dec <- genInvokeDecl tyvars decl 
    def <- genInvokeDef tyvars decl 
    return [dec, def]

--------------------------------------------------------------------------------
