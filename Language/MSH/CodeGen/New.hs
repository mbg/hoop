module Language.MSH.CodeGen.New (
    genNewExp
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.NewExpr 
import Language.MSH.CodeGen.Interop

genNewExp :: NewExpr -> Q Exp 
genNewExp (NewExpr ty args) = do
    return $ (VarE $ mkName $ "_mk" ++ ty)