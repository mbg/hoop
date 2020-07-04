{-# LANGUAGE FlexibleContexts #-}

module Language.MSH.StateEnv where

--------------------------------------------------------------------------------

import Control.Monad.Except

import Data.Graph
import Data.List (intersperse)
import qualified Data.Map as M

import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.Pretty

--------------------------------------------------------------------------------

-- | Represents different errors that can arise during the construction of
-- a class graph.
data StateGraphError 
    = ClassNotFound String
    | CyclicInheritance [String]

instance Show StateGraphError where
    show (ClassNotFound cls)    = "`" ++ cls ++ "' is not in scope."
    show (CyclicInheritance cs) = 
        "The following state classes form a cyclic inheritance hierarchy: " ++ 
        concat (intersperse ", " cs)

--------------------------------------------------------------------------------

type StateEnv = M.Map String StateDecl

ppStateEnv :: StateEnv -> String
ppStateEnv env = render $ vcat $ map (\(k,d) -> pp d) $ M.toList env

--------------------------------------------------------------------------------

-- | `preProcessMethods' @ds builds a method table from a list of decls that
-- make up an object class.
{-preProcessMethods :: [Dec] -> MethodTable
preProcessMethods ds = go emptyMethodTable ds
    where
        go tbl []                                  = tbl
        go tbl (d@(SigD name ty)             : ds) = go (addMethodSig name d tbl) ds
        go tbl (d@(FunD name cs)             : ds) = go (addMethodDef name d tbl) ds
        go tbl (d@(ValD (VarP name) body wh) : ds) = go (addMethodDef name d tbl) ds
        go tbl (d                            : ds) = go tbl ds-}

-- | `inherit` @decl@ extracts the method table from @decl@ (the parent of
-- another state class) and marks all method table entries as inherited.
inherit :: StateDecl -> MethodTable
inherit StateDecl{..} = go stateMethods
    where
        go (MkMethodTable methods) =
            MkMethodTable (M.mapWithKey transform methods)

        -- if @n@ originated in the parent, we inherit it (possibly abstract)
        transform n (GenesisMethod a (Just d) mdef) = InheritedMethod a d mdef
        -- if @n@ was overriden in the parent, we inherit it (never abstract)
        transform n (OverridenMethod dec def) = InheritedMethod False dec (Just def)
        -- otherwise @n@ was inherited by the parent and we continue to inherit it
        transform n d = d

-- | `buildMethodTable` @decl@ builds a method table for @decl@.
buildMethodTable :: StateDecl -> StateDecl
buildMethodTable s@StateDecl{..} =
    s { stateMethods = checkMethodTable s (go initialTable stateBody) }
    where
        initialTable :: MethodTable
        initialTable = case stateParent of
            Just p  -> inherit p
            Nothing -> emptyMethodTable

        go :: MethodTable -> [Dec] -> MethodTable
        go tbl [] = tbl
        -- is this a function signature?
        go tbl (d@(SigD name ty)             : ds)
            | declByParent name s = error $ 
                "Method typing for inherited method `" ++ nameBase name ++ "`"
            | otherwise = go (addMethodSig name d tbl) ds
        -- is this a function/value definition (annoyingly: two different
        -- constructors)
        go tbl (d@(FunD name cs)             : ds)
            | declByParent name s = go (addOverride name d tbl) ds
            | otherwise = go (addGenesisDef name d tbl) ds
        go tbl (d@(ValD (VarP name) body wh) : ds)
            | declByParent name s = go (addOverride name d tbl) ds
            | otherwise = go (addGenesisDef name d tbl) ds
        -- otherwise it is some type of Haskell definition we don't care about;
        -- just ignore it
        go tbl (d                            : ds) = go tbl ds

--------------------------------------------------------------------------------

-- | `checkMethodTable` @decl tbl@ performs sanity checks on @tbl@ for @decl@:
-- this function fails if @tbl@ contains abstract methods and @decl@ is not
-- marked as abstract
checkMethodTable :: StateDecl -> MethodTable -> MethodTable
checkMethodTable s tbl = 
    MkMethodTable $ M.mapWithKey check (methods tbl)
    where check k v | abstractEntry v && not (isAbstractClass s) = 
                        error $ "Abstract member `" ++ k ++ 
                                "` in non-abstract class `" ++
                                stateName s ++ "`"
                    | otherwise = v

--------------------------------------------------------------------------------

-- | `buildStateGraph` @env@ constructs a strongly-connected graph from @env@.
buildStateGraph :: StateEnv -> Except StateGraphError StateEnv
buildStateGraph = {- throwError . ClassNotFound . show . toGraph -} 
    go M.empty . stronglyConnCompR . toGraph
    where
        -- nothing more to add
        go env []                            = return env
        -- we found a cyclic dependency group, raise an error
        go env (CyclicSCC cs           : ds) = 
            throwError $ CyclicInheritance [c | (_,c,_) <- cs]
        -- we found an acyclic node without dependencies, so
        -- we just construct the method table for it
        go env (AcyclicSCC (dec,n,[])  : ds) = 
            go (M.insert n (buildMethodTable dec) env) ds
        -- we found an acyclic node with dependencies, try to resolve
        -- the parent or throw an error if it cannot be found
        go env (AcyclicSCC (dec,n,[p]) : ds) = case M.lookup p env of
            Nothing   -> throwError (ClassNotFound p)
            (Just pd) -> go (M.insert n (buildMethodTable dec') env) ds
                where dec' = dec { stateParent = Just pd }

-- | `toGraph` @env@ turns @env@ into a suitable graph for the SCC algorithm.
toGraph :: StateEnv -> [(StateDecl, String, [String])]
toGraph = map (\(k,v) -> (v, k, dep v)) . M.toList
    where
        -- a state class either has zero dependencies if it is a base class,
        -- or exactly one dependency if it inherits from another class
        dep StateDecl{ stateParentN = Nothing } = []
        dep StateDecl{ stateParentN = Just p  } = [p]

--------------------------------------------------------------------------------
