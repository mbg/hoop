module Language.Hoop.QuasiQuoters where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Hoop.Parsers
import Language.Hoop.CodeGen

state = QuasiQuoter {
    quoteExp  = undefined,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = stateParser
}

stateParser :: String -> Q [Dec]
stateParser code = do
    r <- parseStateDecl code
    genStateDecls r

{-new = QuasiQuoter {
    quoteExp  = newParser,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = undefined
}

newParser :: String -> Q Exp 
newParser code = do
    r <- parseNewExpr code
    genNewExp r-}
    