module Language.MSH.CodeGen.NewInstance (
    genNewInstance
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.Constructor
import Language.MSH.BuiltIn
import Language.MSH.StateDecl
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Shared

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
    return $ InstanceD [] (AppT ct ty) [synInst,eq]