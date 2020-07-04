module Language.MSH.CodeGen.Interop (
    parseType,
    parseDecs,
    parseExp
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.Parser as Exts
import Language.Haskell.Exts.Extension as HS
import Language.Haskell.Meta.Syntax.Translate (toType, toDecs, toExp)

{-
    Utility functions to parse and convert Haskell syntax
-}

-- | Haskell language extensions we want to allow
extensions = map HS.EnableExtension [
    HS.GADTs,
    HS.TypeFamilies,
    HS.RankNTypes,
    HS.FunctionalDependencies,
    HS.ScopedTypeVariables,
    HS.MultiParamTypeClasses,
    HS.FlexibleInstances,
    HS.FlexibleContexts,
    HS.TypeOperators,
    HS.LambdaCase]

-- | Configuration for the Haskell parser
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode "" Haskell2010 extensions True True Nothing True

-- | Parses a string into a TH type
parseType :: String -> Type
parseType = toType . Exts.fromParseResult . Exts.parseTypeWithMode parseMode

parseDecs :: String -> [Dec]
parseDecs xs = let (Syntax.Module _ _ _ _ ds) = Exts.fromParseResult $ Exts.parseModuleWithMode parseMode xs in toDecs ds

parseExp :: String -> Exp
parseExp = toExp . Exts.fromParseResult . Exts.parseExpWithMode parseMode
