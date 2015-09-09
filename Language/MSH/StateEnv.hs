{-# LANGUAGE FlexibleContexts #-}

module Language.MSH.StateEnv where

import Control.Monad.Except

import Data.Graph
import Data.List (intersperse)
import qualified Data.Map as M

import Language.MSH.StateDecl

data StateGraphError = ClassNotFound String 
                     | CyclicInheritance [String]

instance Show StateGraphError where 
    show (ClassNotFound cls)    = "`" ++ cls ++ "' is not in scope."
    show (CyclicInheritance cs) = "The following state classes form a cyclic inheritance hierarchy: " ++ concat (intersperse ", " cs)

type StateEnv = M.Map String StateDecl

-- | `buildStateGraph env' resolves types.
buildStateGraph :: StateEnv -> Except StateGraphError StateEnv
buildStateGraph = go M.empty . stronglyConnCompR . toGraph
    where
        go env []                            = return env
        go env (CyclicSCC cs           : ds) = throwError $ CyclicInheritance [c | (_,c,_) <- cs]
        go env (AcyclicSCC (dec,n,[])  : ds) = go (M.insert n dec env) ds
        go env (AcyclicSCC (dec,n,[p]) : ds) = case M.lookup p env of 
            Nothing   -> throwError (ClassNotFound p)
            (Just pd) -> go (M.insert n (dec { stateParent = Just pd }) env) ds

-- | `toGraph env' turns `env' into a suitable graph for the SCC algorithm.
toGraph :: StateEnv -> [(StateDecl, String, [String])]
toGraph = map (\(k,v) -> (v, k, dep v)) . M.toList 
    where
        -- a state class either has zero dependencies if it is a base class,
        -- or exactly one dependency if it inherits from another class
        dep (StateDecl { stateParentN = Nothing  }) = []
        dep (StateDecl { stateParentN = (Just p) }) = [p]


