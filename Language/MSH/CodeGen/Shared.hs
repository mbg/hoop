module Language.MSH.CodeGen.Shared where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl

renameT :: (String -> String) -> Type -> Type
renameT f (ConT (Name n _)) = ConT $ mkName $ f $ occString n

conName :: Con -> Name
conName (NormalC n _)   = n
conName (RecC n _)      = n
conName (InfixC _ n _)  = n
conName (ForallC _ _ c) = conName c

appN :: Type -> [String] -> Type
appN t []     = t
appN t (a:as) = appN (AppT t (VarT $ mkName a)) as

appN' :: Type -> [Type] -> Type
appN' t [] = t
appN' t (a:as) = appN' (AppT t a) as

appEs :: Exp -> [Exp] -> Exp
appEs f [] = f
appEs f (a:as) = appEs (AppE f a) as

infixr 5 `arr`
arr :: Type -> Type -> Type
arr f a = AppT (AppT ArrowT f) a

tuple2 :: Type -> Type -> Type
tuple2 a b = AppT (AppT (TupleT 2) a) b

tuple :: [Type] -> Type
tuple ts = appN' (TupleT $ length ts) ts

renameParent :: (String -> String) -> Type -> Type 
renameParent f (ConT (Name n _))          = ConT $ mkName $ f $ occString n
renameParent f (AppT (ConT (Name n _)) a) = AppT (ConT $ mkName $ f $ occString n) a

parentName :: Type -> Name 
parentName (ConT n)          = n
parentName (AppT (ConT n) _) = n
parentName _                 = error "parentName: Invalid parent type"

parentArgs :: Type -> [Type]
parentArgs (ConT n)   = []
parentArgs (AppT p a) = parentArgs p ++ [a]

getFields :: [StateMemberDecl] -> [(String, String)]
getFields [] = []
getFields (StateDataDecl n me _ : ds) = case me of
    (Just e) -> (n,e) : getFields ds
    Nothing  -> (n,"undefined") : getFields ds
getFields (_ : ds) = getFields ds

-- | Applies a type `m' to the return type of a function.
wrapMethodType :: Bool -> (Type -> Type) -> Type -> Type 
wrapMethodType False m (ForallT tvs cxt t)  = wrapMethodType False m t
wrapMethodType True  m (ForallT tvs cxt t)  = ForallT tvs cxt $ wrapMethodType True m t
wrapMethodType k m (AppT (AppT ArrowT f) a) = AppT (AppT ArrowT f) (wrapMethodType k m a)
wrapMethodType _ m a                        = m a

unwrapForalls :: Type -> Type -> Type
unwrapForalls (ForallT tvs cxt t) b = ForallT tvs cxt (unwrapForalls t b)
unwrapForalls _ b                   = b

