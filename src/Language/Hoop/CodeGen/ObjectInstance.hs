module Language.Hoop.CodeGen.ObjectInstance (
    genObjectInstance
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.StateDecl
import Language.Hoop.CodeGen.Shared

genObjectTypeInsts :: Type -> Type -> Q [Dec]
genObjectTypeInsts obj st = do
    m <- VarT `fmap` newName "m"
    s <- VarT `fmap` newName "st"
    r <- VarT `fmap` newName "r"
    t <- VarT `fmap` newName "ty"
    return [ TySynInstD (mkName "QueryObject") $ TySynEqn [obj] obj
           , TySynInstD (mkName "QueryMonad")  $ TySynEqn [obj, m] m
           , TySynInstD (mkName "QueryResult") $ TySynEqn [obj, t, s, m, r]
                (foldl AppT (ConT $ mkName "RunnableQuery") [ {-ConT (mkName "ExtCall"),-}
                                                             obj, st, m, r ])]

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
        cl2 = Clause [VarP $ mkName "obj", ConP (mkName "MkField") [VarP $ mkName "eg", WildP, WildP, WildP]] (NormalB $ AppE (ConE $ mkName "MkExtCall") (AppE (VarE $ mkName "eg") (VarE $ mkName "obj"))) []
        eqn = FunD (mkName ".!") [cl1, cl2]
        ds  = [{- ost, -} eqn]
    fams <- genObjectTypeInsts obj st
    return $ InstanceD Nothing cxt ty ds : fams
--genObjectInstance _ = return []
