{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, KindSignatures #-}
{-# LANGUAGE Rank2Types, FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}

module Language.MSH.Selectors where

import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.State

-- | Enumerates call types.
data CallType = ExtCall     -- ^ The call is to a method, externally.

-- | Represents a query which can be run by combinators such as `result', `object', etc.
data RunnableQuery (ty :: CallType) obj st ctx r where
    MkExtCall  :: ctx (r, obj) -> RunnableQuery ExtCall obj st ctx r 

-- | Represents a context in which combinators may be used.
class Functor ctx => CallCtx (ty :: CallType) ctx where 
    type CtxResult ty ctx r :: *

    result :: RunnableQuery ty obj st ctx r -> CtxResult ty ctx r
    object :: RunnableQuery ty obj st ctx r -> CtxResult ty ctx obj 
    --(<:) :: RunnableQuery ty obj ctx r -> r -> ctx ()

{-instance Functor ctx => CallCtx ThisCall ctx where 
    type CtxResult ThisCall ctx r = ()

    getResult _ = ()
    getObject _ = ()-}

instance (ctx ~ Identity) => CallCtx ExtCall ctx where 
    type CtxResult ExtCall ctx r = r

    result (MkExtCall call) = fst $ runIdentity call 
    object (MkExtCall call) = snd $ runIdentity call 
    --(<:) (MkExtCall call) v = undefined

data MemberType = Mutable | Immutable 
data FieldType = Method | Field

type family FieldComposeResult (lhs :: FieldType) (rhs :: FieldType) :: FieldType where
    FieldComposeResult Method Method = Method
    FieldComposeResult Method Field  = Method 
    FieldComposeResult Field  Method = Method
    FieldComposeResult Field  Field  = Field 

data Selector (ty :: FieldType) o s m a where
    MkMethod :: StateT s m a -> 
                (o -> m (a, o)) -> 
                Selector Method o s m a
    MkField  :: (o -> m (a, o)) -> 
                StateT s m a -> 
                (o -> a -> m ((), o)) -> 
                (a -> StateT s m ()) ->
                Selector Field o s m a

data This o s (m :: * -> *) a where 
    MkThis :: This o s m a

type family QueryObject obj :: *
type family QueryMonad obj (m :: * -> *) :: * -> *
type family QueryResult obj (ty :: FieldType) st (m :: * -> *) r :: *

infixr 8 .!
class Monad m => Object obj st m where 
    this :: This obj st m obj
    this = MkThis

    (.!) :: forall r ty.obj -> 
            Selector ty (QueryObject obj) st (QueryMonad obj m) r -> 
            QueryResult obj ty st m r

-- | If `s' returns a value whose type is a `Functor', then `s.$m' calls `m' on the
--   inner value of `s' via `fmap'.
(.$) :: (Monad ctx, Functor f) => Selector lty obj st ctx (f a) -> Selector rty a st' Identity b -> Selector (FieldComposeResult lty rty) obj st ctx (f b)
(MkField eg ig es is) .$ (MkMethod ri re) = MkMethod 
    (ig >>= \x -> let p = fmap (runIdentity . re) x in is (fmap snd p) >> return (fmap fst p)) 
    (\s -> eg s >>= \(x,s') -> let p = fmap (runIdentity . re) x in es s' (fmap snd p) >>= \(_,s'') -> return (fmap fst p, s''))
(MkField eg ig es is) .$ (MkField reg rig res ris) = MkField 
    undefined 
    undefined
    undefined
    undefined
(MkMethod li le) .$ (MkMethod ri re) = MkMethod 
    undefined 
    undefined
(MkMethod li le) .$ (MkField reg rig res ris) = MkMethod 
    undefined 
    undefined 

{- 
type instance QueryMonad (RunnableQuery ThisCall obj st ctx r) ctx' = ctx
type instance QueryObject (RunnableQuery ThisCall obj st ctx r) = obj 

type instance QueryMonad (RunnableQuery Call obj st ctx r) ctx' = Identity
type instance QueryMonad (RunnableQuery IntCall obj st ctx r) ctx' = Identity
type instance QueryMonad (RunnableQuery ExtCall obj st ctx r) ctx' = Identity

type instance QueryObject (RunnableQuery Call obj st ctx r) = r
type instance QueryObject (RunnableQuery IntCall obj st ctx r) = r
type instance QueryObject (RunnableQuery ExtCall obj st ctx r) = r

type instance QueryResult (RunnableQuery ty obj st ctx r) st' m x = 
    RunnableQuery ty obj st m x

instance (Object obj cake ctx, cake ~ cake) =>
    Object (RunnableQuery ThisCall obj cake ctx obj) cake ctx where 

    type ObjSt (RunnableQuery ThisCall obj cake ctx obj) = cake

    (.!) _ _ = undefined

instance (Object obj st ctx, Object r st' Identity, ctx ~ ctx') => 
    Object (RunnableQuery Call obj st ctx r) st' Identity where 

    type ObjSt (RunnableQuery Call obj st ctx r) = st

    (.!) (MkCall li le) (MkMethod ri re) = MkCall undefined undefined
    (.!) (MkCall li le) (MkField ge gi se si) = MkCall undefined undefined

instance (Object obj st ctx, Object r st' Identity, ctx ~ ctx') => 
    Object (RunnableQuery IntCall obj st ctx r) st' ctx' where 

    type ObjSt (RunnableQuery IntCall obj st ctx r) = st

    -- NOTE: This discards the state of the sub-call, which can easily be worked around
    --       by splitting up the query, but maybe we should provide an alternative 
    (.!) (MkIntCall li) (MkMethod _ re) = MkIntCall (li >>= \r -> return $ fst $ runIdentity $ re r)
    --(.!) (MkIntCall li) (MkField ge _ _ _) = MkIntCall (li >>= \r -> )

instance (Object obj st ctx, Object r st' Identity, ctx ~ ctx') => 
    Object (RunnableQuery ExtCall obj st ctx r) st' ctx' where 

    type ObjSt (RunnableQuery ExtCall obj st ctx r) = st

    (.!) (MkExtCall le) (MkMethod _ re) = MkExtCall undefined
-}

-- For fields:
-- * run the internal call (if the selector on the RHS is a method)
-- * run the internal getter (if the selector on the RHS is a field)
type instance QueryMonad (Selector Method obj st ctx r) ctx' = ctx'
type instance QueryObject (Selector Method obj st ctx r) = r 
type instance QueryResult (Selector Method obj st ctx r) ty st' m x = 
    Selector Method obj st ctx x
instance (Object obj st ctx, Object r st' Identity, m ~ Identity) => 
    Object (Selector Method obj st ctx r) st' m where

    (.!) (MkMethod li le) (MkMethod ri re) = MkMethod 
        (li >>= \r -> return $ fst $ runIdentity $ re r) 
        (\s -> le s >>= \(r, obj) -> return (fst $ runIdentity $ re r, obj))

    (.!) (MkMethod li le) (MkField eg ig es is) = MkMethod
        (li >>= \r -> return $ fst $ runIdentity $ eg r)
        (\s -> le s >>= \(r, obj) -> return (fst $ runIdentity $ eg r, obj))

-- For fields:
-- * run the internal call (if the selector on the RHS is a method)
-- * run the internal getter (if the selector on the RHS is a field)
type instance QueryMonad (Selector Field obj st ctx r) ctx' = ctx'
type instance QueryObject (Selector Field obj st ctx r) = r 
type instance QueryResult (Selector Field obj st ctx r) Method st' m x = 
    Selector Method obj st ctx x
type instance QueryResult (Selector Field obj st ctx r) Field st' m x = 
    Selector Field obj st ctx x   
instance (Object obj st ctx, Object r st' Identity, m ~ Identity) =>
    Object (Selector Field obj st ctx r) st' m where 

    -- get the value of the field on the LHS, run the RHS method on it, then
    -- set the value of the field to the object returned by the RHS method
    (.!) (MkField eg ig es is) (MkMethod ri re) = MkMethod 
        (ig >>= \r -> let (r',s) = runIdentity (re r) in is s >> return r') 
        (\s -> eg s >>= \(r',s') -> let (r'',s'') = runIdentity (re r') in es s' s'' >>= \(_,s''') -> return (r'',s'''))

    {-(.!) (MkField eg ig es is) (MkField reg rig res ris) = MkField
        undefined
        undefined
        undefined
        undefined-}

-- For `this':
-- * run the internal call (if the selector on the RHS is a method)
-- * run the internal getter (if the selector on the RHS is a field)
type instance QueryMonad (This obj st ctx r) ctx' = ctx'
type instance QueryObject (This obj st ctx r) = obj 
type instance QueryResult (This obj st ctx r) ty st' m x = StateT st' ctx x
instance (Object obj st ctx, Object r st' ctx', ctx ~ ctx', st ~ st') =>
    Object (This obj st ctx r) st' ctx' where

    (.!) _ (MkMethod ri re)      = ri
    (.!) _ (MkField ge gi se si) = gi
