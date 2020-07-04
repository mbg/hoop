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

module Expr2 where

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

abstract state BinOp : Expr where 
    data left :: Expr 
    data right :: Expr 
    data opr :: Int -> Int -> Int

state Add : BinOp where 
    eval = do 
        x <- this.!left.!eval 
        y <- this.!right.!eval 
        f <- this.!opr 
        return (f x y)
|]

v :: Val
v = new 5

e :: Expr
e = upcast v

a :: Add
a = new (e,e,(+))

b :: BinOp 
b = upcast a

bar = (result $ v.!eval)
    + (result $ a.!eval)
    + (result $ e.!eval)
    + (result $ b.!eval)
