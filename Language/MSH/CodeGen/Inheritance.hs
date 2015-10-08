module Language.MSH.CodeGen.Inheritance where

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl 
import Language.MSH.StateEnv
import Language.MSH.MethodTable
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop

data HasMethodResult = DefResult Bool | ContResult String

class HasMethod a where
    hasMethod :: Name -> a -> Bool

instance HasMethod Dec where
    hasMethod name (SigD n _) = nameBase n == nameBase name
    hasMethod name _          = False

isOverridenEnv :: StateEnv -> StateDecl -> Name -> Q Bool
isOverridenEnv env (StateDecl {
        stateParentN = mp,
        stateBody = body
}) name = case mp of
    Nothing  -> return $ any (hasMethod name) body
    (Just p) -> isInheritedFromParent env p name

{-parentFromInfo :: Cxt -> Maybe String 
parentFromInfo [] = Nothing 
parentFromInfo (ClassP n _ : cs)
    | nameBase n /= "Object" = Just (nameBase n) -- TODO: REmove "Like"?
    | otherwise              = parentFromInfo cs
parentFromInfo (_ : cs) = parentFromInfo cs-}

isInheritedFromInfo :: StateEnv -> Info -> Name -> Q Bool
isInheritedFromInfo env (ClassI (ClassD cxt _ _ _ ds) _) name = error "Inheritance:isInheritedFromInfo" {-case parentFromInfo cxt of
    Nothing -> return $ any (hasMethod name) ds 
    (Just p) -> fail $ show cxt -- TODO: we should search `p'-}

isInheritedFromParent :: StateEnv -> String -> Name -> Q Bool
isInheritedFromParent env p name = let pn = nameBase $ parentName $ parseType p in case M.lookup pn env of 
    (Just s) -> isOverridenEnv env s name
    Nothing  -> do
        mn <- lookupTypeName (pn ++ "Like")
        case mn of
            Nothing  -> fail $ "`" ++ pn ++ "' is not in scope."
            (Just n) -> do
                i <- reify n
                isInheritedFromInfo env i name 

-- | `isInherited env mp name' determines whether a method `name' is inherited from `mp'
isInherited :: StateEnv -> Maybe String -> Name -> Q Bool
isInherited env Nothing  name = return False
isInherited env (Just p) name = isInheritedFromParent env p name

declByParent :: Name -> StateDecl -> Bool
declByParent _ (StateDecl { stateParent = Nothing })  = False 
declByParent n (StateDecl { stateParent = (Just p) }) = 
    M.member (nameBase n) (methodSigs $ stateMethods p) || declByParent n p

-- | Determines whether a method is abstract.
isAbstract :: Name -> StateDecl -> Bool
isAbstract n (StateDecl { stateParent = Nothing, stateMethods = tbl }) = 
    M.notMember (nameBase n) (methodDefs tbl)
isAbstract n (StateDecl { stateParent = Just p, stateMethods = tbl }) =
    M.notMember (nameBase n) (methodDefs tbl) && isAbstract n p
