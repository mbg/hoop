module Language.Hoop.CodeGen.Data where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.StateDecl
import Language.Hoop.CodeGen.Interop

genDataField :: String -> String -> String -> Q VarStrictType
genDataField cl name typ = do
    let
        fname = mkName $ "_" ++ cl ++ "_" ++ name
    return (fname, Bang NoSourceUnpackedness NoSourceStrictness, parseType typ)

genDataFields :: String -> [StateMemberDecl] -> Q [VarStrictType]
genDataFields cl [] = return []
genDataFields cl (StateDataDecl n _ t : ds) = do
    v  <- genDataField cl n t
    vs <- genDataFields cl ds
    return $ v : vs

genStateData :: [TyVarBndr] -> StateDecl -> Q Dec
genStateData tyvars (StateDecl { stateName = name, stateData = ds }) = do
    let
        dname    = mkName $ name ++ "State"
        dctrname = mkName $ "Mk" ++ name ++ "State"
    fs <- genDataFields name ds
    return $ DataD [] dname tyvars Nothing [RecC dctrname fs] []
