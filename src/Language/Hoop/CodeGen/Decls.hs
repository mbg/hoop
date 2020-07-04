{-# LANGUAGE TemplateHaskell #-}

module Language.Hoop.CodeGen.Decls (
    genStateDecls
) where

--------------------------------------------------------------------------------

import Debug.Trace (trace)

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.Except (runExcept)
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

import Language.Hoop.StateDecl
import Language.Hoop.StateEnv
import Language.Hoop.Constructor
import Language.Hoop.Parsers
import Language.Hoop.CodeGen.Shared
import Language.Hoop.CodeGen.Interop
import Language.Hoop.CodeGen.Data
import Language.Hoop.CodeGen.Object
import Language.Hoop.CodeGen.Monad
import Language.Hoop.CodeGen.Class
import Language.Hoop.CodeGen.Instances (genStateInstances)
import Language.Hoop.CodeGen.Methods
import Language.Hoop.CodeGen.Constructors
import Language.Hoop.CodeGen.MiscInstances
import Language.Hoop.CodeGen.Inheritance
import Language.Hoop.CodeGen.Invoke

--------------------------------------------------------------------------------

genIdentityInstance :: Q Dec
genIdentityInstance = do
    let
        ty = tuple []
    return $ InstanceD Nothing [] ty []


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
genStateDecl env s@StateDecl{ stateParams = vars, stateBody = decls } = trace ("---------------\nClass: " ++ stateName s) $ do
    let
        tyvars   = map (PlainTV . mkName) vars
    -- generate the type for the class data
    d  <- genStateData tyvars s
    -- generate lenses
    ls <- makeFieldOpticsForDec stateLensRules d
    -- generate the state monad
    t  <- genStateType tyvars s
    -- generate the object type
    o  <- genStateObject tyvars s
    -- generat the _C_invoke function
    invk <- genInvoke tyvars s
    -- generate the type classes
    c  <- genStateClass env tyvars decls s
    -- generate the type class instances
    is <- genStateInstances env c decls s
    -- generate constructors
    cs <- genConstructors env s
    -- generate misc. type class instances
    misc <- genMiscInstances s o cs
    -- generate method implementations
    ms <- genMethods s (stateName s) vars 
    -- concatenate all the new declarations
    return $ [d,t,o] ++ invk ++ [c] ++ is ++ ls ++ [sctrDec cs] ++ ms ++ misc

genStateDecls :: StateEnv -> Q [Dec]
genStateDecls env = case runExcept $ buildStateGraph env of
    (Left err)   -> fail $ show err
    (Right env') -> do
        runIO $ writeFile "graph.log" (ppStateEnv env')
        dss <- mapM (genStateDecl env') (M.elems env')
        return $ concat dss
