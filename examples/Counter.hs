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

module Counter where

import Language.MSH

[state| 
state Counter where
    data count :: Int
 
    next :: Int 
    next = do
        r <- this.!count 
        count <: (r+1)
        return r

state NameGen : Counter where
    data prefix :: String

    newName :: String 
    newName = do 
        n <- this.!next 
        p <- this.!prefix
        return (p ++ show n)
|]