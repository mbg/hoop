module Language.Hoop.CodeGen.NewInstance (
    genNewInstance
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.Constructor
import Language.Hoop.BuiltIn
import Language.Hoop.StateDecl
import Language.Hoop.CodeGen.Interop
import Language.Hoop.CodeGen.Shared

genNewInstance :: StateCtr -> StateDecl -> Q Dec
genNewInstance (SCtr (FunD cn [Clause ps _ _]) ts) (StateDecl {
    stateName = name,
    stateParams = vars
}) = do
    ns <- mapM (\(VarP n) -> return n) ps
    let
        ct      = ConT $ mkName newClassName
        ty      = appN (ConT $ mkName name) vars
        appCtr  = appEs (VarE cn) (map VarE ns)
        synInst = TySynInstD (mkName newArgsTypeName) $ TySynEqn [ty] (tuple ts)
        eq      = FunD (mkName newKwdName) [Clause [TupP ps] (NormalB appCtr) []]
    return $ InstanceD Nothing [] (AppT ct ty) [synInst,eq]
