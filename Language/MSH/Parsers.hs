module Language.MSH.Parsers (
    parseStateDecl,
    parseNewExpr
) where

import Language.Haskell.TH

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Control.Monad (void)

import Data.Char (isSpace)
import Data.Text (pack, unpack, strip)
import qualified Data.Map as M

import Language.MSH.StateDecl
import Language.MSH.NewExpr
import Language.MSH.CodeGen.Interop (parseDecs)

import Debug.Trace

trim :: String -> String
trim = unpack . strip . pack

isSpaceNoNL :: GenParser Char a Char
isSpaceNoNL = satisfy (\c -> isSpace c && c /= '\n' && c /= '\r')

-- | Parses state declarations
parseStateDecl :: String -> Q (M.Map String StateDecl)
parseStateDecl code = case parse stateDecls "" code of
    (Left err) -> fail $ show err
    (Right r)  -> return r

parseNewExpr :: String -> Q NewExpr
parseNewExpr code = case parse newExpr "" code of
    (Left err) -> fail $ show err
    (Right r)  -> return r

-- | Parses a variable identifier (starting with a lower-case character)
varid :: GenParser Char a String
varid = do
    c  <- lower
    cs <- many (alphaNum <|> char '\'')
    return (c:cs)

-- | Parses a type/constructor identifier (starting with an upper-case character)
ctrid :: GenParser Char a String
ctrid = do
    c  <- upper
    cs <- many (alphaNum <|> char '\'')
    return (c:cs)

tyVar :: GenParser Char a String
tyVar = do
    v <- varid
    if v == "where" then fail "is keyword"
    else do
        spaces
        return v

abstract :: GenParser Char a (Maybe StateMod)
abstract = string "abstract" >> return (Just Abstract)

final :: GenParser Char a (Maybe StateMod)
final = string "final" >> return (Just Final)

classModifier :: GenParser Char a (Maybe StateMod)
classModifier = abstract <|> final <|> return Nothing

parentClass :: GenParser Char a (Maybe String, [String])
parentClass = (do char ':'
                  spaces
                  ctr <- ctrid
                  spaces
                  tyvars <- many (try tyVar)
                  spaces
                  string "where"
                  return $ (Just ctr,tyvars))
          <|> (string "where" >> return (Nothing, []))

dataInit :: GenParser Char a String
dataInit = do
    string "="
    spaces
    r <- manyTill anyChar (try $ string "::") -- TODO: improve this, so that it takes the last ::
    return r

dataDecl :: GenParser Char a StateMemberDecl
dataDecl = do
    string "data"
    spaces
    id <- varid
    spaces
    val <- optionMaybe dataInit
    case val of
        Nothing   -> string "::"
        otherwise -> return ""
    spaces
    ty <- manyTill anyChar (try $ (void newline) <|> eof)
    return $ StateDataDecl {
        stateDataName = id,
        stateDataExpr = val,
        stateDataType = ty
    }

valueLine :: GenParser Char a String
valueLine = do
    ws <- many1 isSpaceNoNL
    rs <- manyTill anyChar $ try (void endOfLine <|> eof)
    return (ws ++ rs ++ "\r\n")

emptyLine :: GenParser Char a String
emptyLine = do
    many isSpaceNoNL
    void endOfLine {-<|> eof-}
    return "\n"

valueDecl :: GenParser Char a String
valueDecl = do
    ls <- many (valueLine <|> emptyLine)
    --error $ concat ls
    return $ concat ls

stateMember :: GenParser Char a StateMemberDecl
stateMember = do
    spaces
    dataDecl

stateDecl :: GenParser Char a StateDecl
stateDecl = do
    spaces
    mod <- classModifier
    spaces
    string "state"
    spaces
    id <- ctrid
    spaces
    tyvars <- many (try tyVar)
    spaces
    (p,ps) <- parentClass
    many isSpaceNoNL
    many newline
    ms <- many $ try stateMember
    vm <- valueDecl
    let
        body = parseDecs vm
    return $ StateDecl {
        stateMod      = mod,
        stateName     = trim id,
        stateParams   = tyvars,
        stateParentN  = p,
        stateParentPs = ps,
        stateParent   = Nothing,
        stateData     = ms,
        stateBody     = body,
        stateMethods  = emptyMethodTable -- preProcessMethods body
    }

stateDecls :: GenParser Char a (M.Map String StateDecl)
stateDecls = do
    ds <- many stateDecl
    return $ M.fromList [(stateName d ,d) | d <- ds]

newExpr :: GenParser Char a NewExpr
newExpr = do
    spaces
    id   <- ctrid
    spaces
    args <- many anyChar
    return $ NewExpr id args
