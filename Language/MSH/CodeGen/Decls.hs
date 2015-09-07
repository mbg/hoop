{-# LANGUAGE TemplateHaskell #-}

module Language.MSH.CodeGen.Decls (
    genStateDecls
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
--import Control.Monad.State

import Data.Char (toLower)
import Data.Graph (stronglyConnComp)
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Lens.TH
import Control.Lens.Internal.FieldTH

-- needed to parse Haskell syntax and to convert it into TH syntax
import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.Parser as Exts
import Language.Haskell.Exts.Extension
import Language.Haskell.Meta.Syntax.Translate (toType, toDecs, toExp)

import Language.MSH.StateDecl
import Language.MSH.Constructor
import Language.MSH.Parsers
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop
import Language.MSH.CodeGen.Data
import Language.MSH.CodeGen.Object
import Language.MSH.CodeGen.Monad
import Language.MSH.CodeGen.Class
import Language.MSH.CodeGen.Instances
import Language.MSH.CodeGen.Methods
import Language.MSH.CodeGen.Constructors
import Language.MSH.CodeGen.MiscInstances
import Language.MSH.CodeGen.Inheritance

genIdentityInstance :: Q Dec 
genIdentityInstance = do
    let
        ty = tuple []
    return $ InstanceD [] ty []


{-
    External interface
-}

-- | Appends "_lens" to the lens names
lensLookup :: Name -> [Name] -> Name -> [DefName]
lensLookup _ fs field = [TopName $ mkName $ nameBase field ++ "_lens"] 

stateLensRules :: LensRules
stateLensRules = lensRules -- { _fieldToDef = lensLookup }

-- | Generates top-level declarations for a state declaration
genStateDecl :: StateEnv -> StateDecl -> Q [Dec]
genStateDecl env s@(StateDecl m name vars p ds decls) = do
    let
        tyvars   = map (PlainTV . mkName) vars
        decs     = parseDecs decls
    d  <- genStateData tyvars s
    ls <- makeFieldOpticsForDec stateLensRules d
    t  <- genStateType tyvars s
    o  <- genStateObject tyvars s
    c  <- genStateClass env tyvars decs s
    is <- genStateInstances env c decs s
    cs <- genConstructors env s
    misc <- genMiscInstances s o cs
    ms <- genMethods env name vars decs
    return $ [d,t,o,c] ++ is ++ ls ++ [sctrDec cs] ++ ms ++ misc

genStateDecls :: StateEnv -> Q [Dec]
genStateDecls env = do
    dss <- mapM (genStateDecl env) (M.elems env)
    return $ concat dss