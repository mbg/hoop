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

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Stack where

import Language.Hoop

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
        xs <- this.!stack
        stack <: (tail xs)
        return (head xs)
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

example :: Int 
example = let s = object (emptyStack.!pushMany [1,2,3])
          in result (s.!pop)