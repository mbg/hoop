{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Expr where

import Language.MSH

[state| 
abstract state Expr where
    eval :: Int

state Val : Expr where
    data val = 0 :: Int

    eval :: Int
    eval = do
        r <- this.!val
        return r

state Add : Expr where
    data left  :: Expr
    data right :: Expr

    eval :: Int
    eval = do
        l <- this.!left.!eval 
        r <- this.!right.!eval
        return (l+r)
|]

v :: Val
v = new 5

e :: Expr 
e = downcast v

a :: Add
a = new (e,e)

bar = (result $ v.!eval)
    + (result $ a.!eval)
    + (result $ e.!eval)
