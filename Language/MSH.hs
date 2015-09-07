{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures, FlexibleInstances, DefaultSignatures #-}
module Language.MSH (
    module Control.Lens,
    module Control.Monad.Identity,
    module Control.Monad.State,
    module Language.MSH.QuasiQuoters,
    module Language.MSH.Selectors,
    --CallR(..),
    --ObjectCtx(..),
    --Selectable(..),
    -- Selection(..),
    SetterContext(..),
    ValueContext(..),
    HasData(..),
    --Object(..),
    Cast(..),
    New(..),
    Void,
    --value,
    --object,
    --result,
    switch,
    (<:)
) where

import Control.Lens
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Language.MSH.QuasiQuoters
import Language.MSH.Selectors

class HasData obj d | obj -> d where
    extractData :: obj -> d 

class Cast sup sub | sub -> sup where
    downcast :: sub -> sup

class New obj where
    type Args obj :: *
    new :: Args obj -> obj

type Void = ()

{-data MethodR o s m arg ret = MkM {
    mInternal :: arg -> State s m ret,
    mExternal :: obj -> arg -> m (ret, o)
}

data FieldR o s m arg ret = MkF {
    
} -}

{-data CallR obj (m :: * -> *) a b = MkC {
    rCall :: a -> (b, obj)
} | MkF {
    rGet :: (b, obj),
    rSet :: a -> ((), obj)
}

class ObjectCtx m where 
    type ObjCtx m r :: *
    object :: CallR obj m arg ret -> arg -> ObjCtx m obj 
    result :: CallR obj m arg ret -> arg -> ObjCtx m ret 

instance ObjectCtx Identity where
    type ObjCtx Identity r = r
    object (MkC call) = snd . call 
    result (MkC call) = fst . call

instance ObjectCtx (StateT s m) where
    type ObjCtx (StateT s m) r = StateT s m r 
    object = undefined
    result = undefined-}

{-type family App fun arg
data I = I

type instance App I arg = arg

class Invokable f ctx where
    result' :: f obj a b -> a -> App ctx b
    object' :: f obj a b -> a -> App ctx obj

instance Invokable CallR I where
    result' (MkC call) = fst . call
    object' (MkC call) = snd . call-}



{-result :: Selector o s Identity arg ret -> arg -> ret
result (MkCall call)        = fst . call 

object :: Selector o s Identity arg ret -> arg -> o
object (MkCall call)  = snd . call-}

{-result :: CallR obj m arg ret -> arg -> ret
result (MkC call)        = fst . call 

object :: CallR obj m arg ret -> arg -> obj
object (MkC call)  = snd . call-}

--value :: Selector ty o s m val -> StateT s m val 
--value (MkField g g' s s') = g'

--infixr 8 .!
--class Monad m => Object obj st m | obj -> m, obj -> st, st -> obj where
--    (.!) :: obj -> Selector obj s m arg ret -> CallR obj m arg ret 
    {-default (.!) :: Object obj s Identity => obj -> Selector obj s Identity arg ret -> CallR obj Identity arg ret 
    obj .! (MkMethod int ext) = MkC $ \arg -> runIdentity $ ext obj arg
    obj .! (MkField g _ s _) = MkF (runIdentity $ g obj) (\arg -> runIdentity $ s obj arg)-}

{-class Selectable lhs rhs where 
    type Selection lhs rhs :: *
    (.%) :: lhs -> rhs -> Selection lhs rhs 

instance Selectable lhs (Selector lhs s Identity arg ret) where
    type Selection lhs (Selector lhs s Identity arg ret) = CallR lhs Identity arg ret 
    obj .% (MkMethod int ext) = MkC $ \arg -> runIdentity $ ext obj arg
    obj .% (MkField g _ s _) = MkF (runIdentity $ g obj) (\arg -> runIdentity $ s obj arg)
-}
{-instance Object a Identity where
    obj .! (MkMethod int ext) = MkC $ \arg -> runIdentity $ ext obj arg
    obj .! (MkField g _ s _) = MkF (runIdentity $ g obj) (\arg -> runIdentity $ s obj arg)-}

{-class Selection a b c | b -> a where
    (.%) :: a -> b -> c

instance Selection obj (Selector obj s Identity arg ret) (Selector obj s m arg ret) where
    obj .% (MkMethod int ext) = MkCall $ \arg -> runIdentity $ ext obj arg
-}
{-instance Object (Selector o s m arg ret) Identity where
    (MkMethod int ext) .% (MkMethod int' ext') = MkMethod int' (\obj arg -> ext')-}

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

{-instance SetterContext o o s m where 
    (MkCall call) <: v = snd $ call v-}
--instance ValueContext o val where 
--    type VCSt o =

(<:) :: Monad m => Selector Field o s m val -> val -> StateT s m ()
(MkField g g' s s') <: v = s' v

{-value :: Selector o s m arg val -> StateT s m val 
value (MkField g g' s s') = g'-}

switch :: Monad m => Selector ty o s m val -> (val -> StateT s m b) -> StateT s m b
switch (MkMethod im em)      m = im >>= m
switch (MkField eg ig es is) m = ig >>= m
