{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures, FlexibleInstances, DefaultSignatures #-}
module Language.Hoop (
    module Control.Lens,
    module Control.Monad.Identity,
    module Control.Monad.State,
    module Language.Hoop.QuasiQuoters,
    module Language.Hoop.Selectors,
    module Language.Hoop.RuntimeError,

    SetterContext(..),
    ValueContext(..),
    HasData(..),
    Cast(..),
    New(..),
    Void,
    switch,
    (<:),
    Identity(..)
) where

import Control.Lens
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Control.Monad.Fail
import Language.Hoop.QuasiQuoters
import Language.Hoop.Selectors
import Language.Hoop.RuntimeError

class HasData obj d | obj -> d where
    extractData :: obj -> d

class Cast sub sup | sub -> sup where
    upcast :: sub -> sup

class New obj where
    type Args obj :: *
    new :: Args obj -> obj

type Void = ()

infixl 7 <:
class SetterContext r o s m where
    (>:) :: Selector Field o s m val -> val -> r

class ValueContext r val where
    type VCSt  r :: *
    type VCM   r :: * -> *
    --value :: Selector o (VCSt r) (VCM r) arg val -> r

{-instance SetterContext (StateT s m ()) o s m where
    (MkField g g' s s') <: v = s' v-}

instance ValueContext (StateT s m val) val where
    type VCSt (StateT s m val) = s
    type VCM (StateT s m val) = m
    --value (MkField g g' s s') = g'


(<:) :: Monad m => Selector Field o s m val -> val -> StateT s m ()
(MkField g g' s s') <: v = s' v

switch :: Monad m => Selector ty o s m val -> (val -> StateT s m b) -> StateT s m b
switch (MkMethod im em)      m = im >>= m
switch (MkField eg ig es is) m = ig >>= m

instance MonadFail Identity where 
    fail = error 