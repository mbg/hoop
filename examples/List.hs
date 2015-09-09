{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module List where

import Language.MSH

[state| 
state MListItem a where
    data val  = error "val" :: a
    data next = Nothing     :: Maybe (MListItem a)

    insertEnd :: MListItem a -> MListItem a  
    insertEnd item = do
        switch next $ \x -> case x of
            Nothing  -> next <: Just item 
            (Just n) -> next <: Just (object (n.!insertEnd item))
        return item

    toListItems :: [a]
    toListItems = do
        v <- this.!val
        switch next $ \x -> case x of 
            Nothing  -> return [v]
            (Just n) -> do
                let
                    vs = result (n.!toListItems)
                return (v:vs)

state MList a where
    data root = Nothing :: Maybe (MListItem a)

    insert :: a -> Void
    insert val = do 
        let 
            item = new (val, Nothing)
        switch root $ \x -> case x of
            Nothing  -> root <: Just item
            (Just r) -> root <: Just (object (r.!insertEnd item))

    toList :: [a]
    toList = do 
        switch root $ \x -> case x of
            Nothing  -> return []
            (Just r) -> return $ result (r.!toListItems)

state Program where 
    data l :: MList Int 

    run :: MList Int -> [Int]
    run arg = do 
        l <: arg
        this.!l.!insert 23
        this.!l.!insert 16
        this.!l.!insert 42
        this.!l.!insert 24
        r <- this.!l.!toList
        return r
|]

test :: MList Int -> [Int]
test l = let
    a = object (l.!insert 23)
    b = object (a.!insert 16)
    c = object (b.!insert 42)
    d = object (c.!insert 24) in result (d.!toList) 

foo :: MListItem Int 
foo = new (5, Nothing)

bar = result $ foo.!insertEnd foo.!toListItems

baz :: Program
baz = new (new Nothing)
