module Language.MSH.CodeGen.Interop (
    parseType,
    parseDecs,
    parseExp
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.Parser as Exts
import Language.Haskell.Exts.Extension
import Language.Haskell.Meta.Syntax.Translate (toType, toDecs, toExp)

{-
    Utility functions to parse and convert Haskell syntax
-}

-- | Haskell language extensions we want to allow
extensions = map EnableExtension [GADTs, 
    TypeFamilies, 
    RankNTypes, 
    FunctionalDependencies, 
    ScopedTypeVariables,
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts,
    TypeOperators,
    LambdaCase]

-- | Configuration for the Haskell parser
parseMode = Exts.ParseMode "" Haskell2010 extensions True True Nothing

-- | Parses a string into a TH type
parseType :: String -> Type 
parseType = toType . Exts.fromParseResult . Exts.parseTypeWithMode parseMode

parseDecs :: String -> [Dec]
parseDecs xs = let (Syntax.Module _ _ _ _ _ _ ds) = Exts.fromParseResult $ Exts.parseModuleWithMode parseMode xs in toDecs ds

parseExp :: String -> Exp 
parseExp = toExp . Exts.fromParseResult . Exts.parseExpWithMode parseMode
