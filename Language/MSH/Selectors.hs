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
data CallType = Call        -- ^ The call is to a method, internally or externally.
              | ThisCall    -- ^ The query refers to the current object.
              | IntCall     -- ^ The call is to a method, internally.
              | ExtCall     -- ^ The call is to a method, externally.
              | FieldCall   -- ^ The query refers to a field.

-- | Represents a query which can be run by combinators such as `result', `object', etc.
data RunnableQuery (ty :: CallType) obj st ctx r where
    MkCall     :: StateT st ctx r -> ctx (r, obj) -> RunnableQuery Call obj st ctx r 
    MkThisCall :: RunnableQuery ThisCall obj st ctx r
    MkIntCall  :: StateT st ctx r -> RunnableQuery IntCall obj st (StateT st ctx) r 
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

{-instance Functor ctx => CallCtx IntCall ctx where
    type CtxResult IntCall ctx r = ctx r

    getResult (MkIntCall call) = call-}
    --getObject (MkIntCall call) = call

data FieldType = Method | Field | This

data Selector (ty :: FieldType) o s m a where
    MkMethod :: StateT s m a -> (o -> m (a, o)) -> Selector Method o s m a
    MkField  :: (o -> m (a, o)) -> 
                StateT s m a -> 
                (o -> a -> m ((), o)) -> 
                (a -> StateT s m ()) ->
                Selector Field o s m a
    MkThis   :: Selector This o s m a

type family QueryObject obj :: *
type family QueryMonad obj (m :: * -> *) :: * -> *
type family QueryResult obj st (m :: * -> *) r :: *

infixr 8 .!
class Monad m => Object obj st m where 
    type ObjSt obj :: *

    this :: Selector This obj st m obj 
    this = MkThis

    (.!) :: forall r ty.obj -> 
            Selector ty (QueryObject obj) st (QueryMonad obj m) r -> 
            QueryResult obj st m r

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

{-
    LHS: Selector
-}

type instance QueryMonad (Selector ty obj st ctx r) ctx' = ctx'

type instance QueryObject (Selector Method obj st ctx r) = r 
type instance QueryResult (Selector Method obj st ctx r) st' m x = 
    Selector Method obj st ctx x
instance (Object obj st ctx, Object r st' Identity, m ~ Identity) => 
    Object (Selector Method obj st ctx r) st' m where

    (.!) (MkMethod li le) (MkMethod ri re) = MkMethod 
        (li >>= \r -> return $ fst $ runIdentity $ re r) 
        (\s -> le s >>= \(r, obj) -> return (fst $ runIdentity $ re r, obj))

type instance QueryObject (Selector Field obj st ctx r) = r 
type instance QueryResult (Selector Field obj st ctx r) st' m x = 
    Selector Method obj st ctx x
instance (Object obj st ctx, Object r st' Identity, m ~ Identity) =>
    Object (Selector Field obj st ctx r) st' m where 

    (.!) (MkField eg ig es is) (MkMethod ri re) = MkMethod 
        (ig >>= \r -> let (r',s) = runIdentity (re r) in is s >> return r') 
        undefined
        -- (\s -> eg s >>= \(r',s') -> let (r'',s'') = runIdentity (re r') in es s' s'' >> return r'')

type instance QueryObject (Selector This obj st ctx r) = obj 
type instance QueryResult (Selector This obj st ctx r) st' m x = 
    StateT st' ctx x
instance (Object obj st ctx, Object r st' ctx', ctx ~ ctx', st ~ st') => 
    Object (Selector This obj st ctx r) st' ctx' where

    (.!) _ (MkMethod ri re)      = ri
    (.!) _ (MkField ge gi se si) = gi
