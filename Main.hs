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

module Main where

import Language.MSH


[state| 
state MListItem a where
    data val  = error "val" :: a
    data next = Nothing :: Maybe (MListItem a)

    insertEnd :: MListItem a -> Void
    insertEnd item = do 
        switch next $ \x -> case x of
            Nothing  -> next <: Just item
            (Just n) -> next <: Just (object (n.%insertEnd) item)

    toListItems :: [a]
    toListItems = do 
        v <- value val
        switch next $ \x -> case x of 
            Nothing  -> do
                return [v]
            (Just n) -> do 
                let
                    vs = result (n.%toListItems) ()
                return (v:vs)

state MList a where
    data root = Nothing :: Maybe (MListItem a)

    insert :: a -> Void
    insert val = do 
        let
            item = new () -- val
        switch root $ \x -> case x of
            Nothing  -> root <: Just item
            (Just r) -> root <: Just (object (r.%insertEnd) item)

    toList :: [a]
    toList = do 
        switch root $ \x -> case x of
            Nothing  -> return []
            (Just r) -> return $ result (r.%toListItems) ()

state SList a : MList a where
    data predicate :: a -> a -> Bool

    --insert :: a -> Void
    --insert val = undefined
|]

mlist_insert :: MListLike e s m a => a -> StateT (s a) m ()
mlist_insert v = do
    let
        item = new ()
    switch root $ \case
        Nothing  -> root <: Just item 
        (Just r) -> root <: Just (object (r.%insertEnd) item) 

instance New (MListItem a) where
    type Args (MListItem a) = ()
    new = const _mkMListItem

instance New (MList a) where
    type Args (MList a) = ()
    new = const _mkMList

instance New (SList a) where
    type Args (SList a) = ()
    new = const _mkSList

test :: MList Int -> [Int]
test l = let
    a = object (l.%insert) 23
    b = object (a.%insert) 16
    c = object (b.%insert) 42
    d = object (c.%insert) 24 in result (d.%toList) () 

main = do
    print (test $ new ())
    --print (test $ downcast $ new ()) 
