{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, KindSignatures #-}
{-# LANGUAGE Rank2Types, FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.MSH.Selectors where

--------------------------------------------------------------------------------

import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.State

--------------------------------------------------------------------------------

-- | Represents a query which can be run by combinators such as `result',
-- `object', etc.
data RunnableQuery obj st ctx r where
    MkExtCall  :: ctx (r, obj) -> RunnableQuery obj st ctx r

-- | `result` @selector obtains the result from invoking @selector.
result :: (ctx ~ Identity) => RunnableQuery obj st ctx r -> r
result (MkExtCall call) = fst $ runIdentity call

-- | `object` @selector obtains an updated object from invoking @selector.
object :: (ctx ~ Identity) => RunnableQuery obj st ctx r -> obj
object (MkExtCall call) = snd $ runIdentity call

-- | Enumerates types of object class members.
data MemberType = Method | Field

-- | Determines the type of selector that results from composing two
-- selectors.
type family MemberComposeResult (lhs :: MemberType) (rhs :: MemberType) :: MemberType where
    MemberComposeResult Method Method = Method
    MemberComposeResult Method Field  = Method
    MemberComposeResult Field  Method = Method
    MemberComposeResult Field  Field  = Field

--type Selector ty o s m a =

-- | A selector is a container for object class members.
data Selector (ty :: MemberType) o s m a where
    MkMethod :: StateT s m a ->
                (o -> m (a, o)) ->
                Selector Method o s m a
    MkField  :: (o -> m (a, o)) ->
                StateT s m a ->
                (o -> a -> m ((), o)) ->
                (a -> StateT s m ()) ->
                Selector Field o s m a

-- | Proxy type used to capture the type variables of an object.
data This o s (m :: * -> *) a where
    MkThis :: This o s m a

type family QueryObject obj :: *
type family QueryMonad obj (m :: * -> *) :: * -> *
type family QueryResult obj (ty :: MemberType) st (m :: * -> *) r :: *


infixr 8 .!
class Monad m => Object obj st m where

    this :: This obj st m obj
    this = MkThis

    (.!) :: forall r ty. obj ->
            Selector ty (QueryObject obj) st {-(QueryMonad obj m)-} m r ->
            QueryResult obj ty st m r

-- | If `s' returns a value whose type is a `Functor', then `s.$m' calls `m' on the
--   inner value of `s' via `fmap'.
(.$) :: (Monad ctx, Functor f) =>
        Selector lty obj st ctx (f a) ->
        Selector rty a st' Identity b ->
        Selector (MemberComposeResult lty rty) obj st ctx (f b)
(MkField eg ig es is) .$ (MkMethod ri re) = MkMethod
    (do box <- ig
        let r = fmap (runIdentity . re) box
        is (fmap snd r)
        return (fmap fst r))
    (\this -> do (box,this') <- eg this
                 let r = fmap (runIdentity . re) box
                 (_,this'') <- es this' (fmap snd r)
                 return (fmap fst r, this''))
(MkField eg ig es is) .$ (MkField reg rig res ris) = MkField
    (\this -> do (box,this') <- eg this
                 let r = fmap (runIdentity . reg) box
                 (_,this'') <- es this' (fmap snd r)
                 return (fmap fst r, this''))
    (do box <- ig
        let r = fmap (runIdentity . reg) box
        is (fmap snd r)
        return (fmap fst r))
    -- TODO: the definition of Selector is too restrictive to
    -- implement the following two cases: we need to split the 'a' type
    -- parameter into two: one for the type of values to get and one for
    -- the type of values to set
    undefined
    {-(\this v -> do (box,this') <- eg this
                   let r = fmap (\obj -> runIdentity (res obj v)) box
                   (_,this'') <- es this' (fmap snd r)
                   return ((), this''))-}
    undefined
    {-(\v -> do box <- ig
              )-}
(MkMethod li le) .$ (MkMethod _ re) = MkMethod
    (do box <- li
        let r = fmap (runIdentity . re) box
        return (fmap fst r))
    (\this -> do (box, this') <- le this
                 let r = fmap (runIdentity . re) box
                 return (fmap fst r, this'))
(MkMethod li le) .$ (MkField reg _ _ _) = MkMethod
    (do box <- li
        let r = fmap (runIdentity . reg) box
        return (fmap fst r))
    (\this -> do (box, this') <- le this
                 let r = fmap (runIdentity . reg) box
                 return (fmap fst r, this'))

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
        (do obj <- li
            let (r,obj') = runIdentity (re obj)
            return r)
        (\this -> do (obj,this') <- le this
                     let (r,obj') = runIdentity (re obj)
                     return (r, this'))

    (.!) (MkMethod li le) (MkField eg ig es is) = MkMethod
        (do obj <- li
            let (r,obj') = runIdentity (eg obj)
            return r)
        (\this -> do (obj, this') <- le this
                     let (r,obj') = runIdentity (eg obj)
                     return (r, this'))

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
        (do obj <- ig
            -- * obj is the value (an object) of the field on the left
            -- * use obj as object for the method on the right
            let (r,obj') = runIdentity (re obj)
            -- * update the current object with obj', the updated obj
            is obj'
            -- * return the result of the method on the right
            return r)
        (\this -> do (obj,this') <- eg this
                     -- * use obj as object for the method on the right
                     let (r,obj') = runIdentity (re obj)
                     -- * update the current object with s, the updated r
                     (_,this'') <- es this' obj'
                     return (r,this''))

    (.!) (MkField eg ig es is) (MkField reg rig res ris) = MkField
        (\this -> do (obj,this') <- eg this
                     let (r,obj') = runIdentity (reg obj)
                     (_,this'') <- es this' obj'
                     return (r,this''))
        (do obj <- ig
            let (r,obj') = runIdentity (reg obj)
            is obj'
            return r)
        (\this v -> do (obj,this') <- eg this
                       let (r,obj') = runIdentity (res obj v)
                       (_,this'') <- es this' obj'
                       return (r,this''))
        (\v -> do obj <- ig
                  let (r,obj') = runIdentity (res obj v)
                  is obj'
                  return r)

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
