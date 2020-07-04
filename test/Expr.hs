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
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Expr where

--------------------------------------------------------------------------------

import Language.Hoop

--------------------------------------------------------------------------------

[state|
abstract state Expr where
    eval :: Int

state Val : Expr where
    data val = 0 :: Int

    eval = do
        r <- this.!val
        return r

state Add : Expr where 
    data left :: Expr 
    data right :: Expr 

    eval = do 
        x <- this.!left.!eval 
        y <- this.!right.!eval 
        return (x+y)

state Mul : Expr where
    data mleft  :: Expr
    data mright :: Expr

    eval = do
        l <- this.!mleft.!eval
        r <- this.!mright.!eval
        return (l*r)
|]

v :: Val
v = new 5

e :: Expr
e = upcast v

a :: Add
a = new (e,e)

bar = (result $ v.!eval)
    + (result $ a.!eval)
    + (result $ e.!eval)
