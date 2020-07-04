module Language.Hoop.CodeGen.New (
    genNewExp
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.NewExpr 
import Language.Hoop.CodeGen.Interop

genNewExp :: NewExpr -> Q Exp 
genNewExp (NewExpr ty args) = do
    return $ (VarE $ mkName $ "_mk" ++ ty)