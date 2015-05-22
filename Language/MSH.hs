{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures, FlexibleInstances #-}
module Language.MSH (
    module Control.Lens,
    module Control.Monad.Identity,
    module Control.Monad.State,
    module Language.MSH.QuasiQuoters,
    HasData(..),
    Object(..),
    Cast(..),
    New(..),
    Void,
    Selector(..),
    Field,
    value,
    object,
    result,
    switch,
    (<:)
) where

import Control.Lens
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Language.MSH.QuasiQuoters

class HasData obj d | obj -> d where
    extractData :: obj -> d 

class Cast sup sub | sub -> sup where
    downcast :: sub -> sup

class New obj where
    type Args obj :: *
    new :: Args obj -> obj

type Void = ()
type Id a = a

data Selector o s (m :: * -> *) arg ret = MkMethod { 
    mInternal :: arg -> StateT s m ret,
    mExternal :: o -> arg -> m (ret, o) 
} | MkField {
    fGet  :: o -> m (ret, o),
    fGet' :: StateT s m ret,
    fSet  :: o -> arg -> m ((), o),
    fSet' :: arg -> StateT s m ()
} | MkCall {
    cCall :: arg -> (ret, o)
}

type Field o s m a = Selector o s m a a

result :: Selector o s Identity arg ret -> arg -> ret
result (MkCall call) arg = fst (call arg)

object :: Selector o s Identity arg ret -> arg -> o
object (MkCall call) arg = snd (call arg)

infixr 8 .%
class Monad m => Object obj m where
    (.%) :: obj -> Selector obj s m arg ret -> Selector obj s m arg ret 

instance Object a Identity where
    obj .% (MkMethod int ext) = MkCall $ \arg -> runIdentity $ ext obj arg


infixl 7 <:
(<:) :: Monad m => Field o s m val -> val -> StateT s m ()
(MkField g g' s s') <: v = s' v

value :: Selector o s m arg val -> StateT s m val 
value (MkField g g' s s') = g'

switch :: Monad m => Selector o s m arg val -> (val -> StateT s m b) -> StateT s m b
switch s m = value s >>= m
