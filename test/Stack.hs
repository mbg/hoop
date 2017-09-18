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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Stack where

import Language.MSH

[state| 
state Stack a where
    data stack :: [a]

    push :: a -> Void
    push x = do
        st <- this.!stack
        stack <: (x : st)

    pushMany :: [a] -> Void 
    pushMany = mapM_ (\x -> this.!push x)
 
    pop :: a
    pop = do
        (x:xs) <- this.!stack
        stack <: xs
        return x
|]

instance Show a => Show (Stack a) where 
    show s = show $ result (s.!stack)

pattern Stack :: [a] -> Stack a
pattern Stack xs <- (extractData -> MkStackState xs) where 
    Stack xs = new xs

emptyStack :: Stack a 
emptyStack = Stack []

isEmptyStack :: Stack a -> Bool
isEmptyStack (Stack []) = True
isEmptyStack (Stack _)  = False