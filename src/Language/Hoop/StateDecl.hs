--------------------------------------------------------------------------------

module Language.Hoop.StateDecl (
    module Language.Hoop.MethodTable,

    StateMod(..),
    StateObjCtr(..),
    StateMemberDecl(..),
    StateDecl(..),

    isBaseClass,
    isAbstractClass,
    isFinalClass,
    ctrsForClass,

    declByParent
) where

--------------------------------------------------------------------------------

import Prelude hiding ((<>))
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Ppr as Ppr

import Language.Hoop.MethodTable
import Language.Hoop.Pretty

--------------------------------------------------------------------------------

-- | Enumerates class modifiers.
data StateMod = Abstract | Final deriving Show

-- | Enumerates object constructor types.
data StateObjCtr = DataCtr | StartCtr | MiddleCtr | EndCtr deriving Show

-- | Used to represent fields in state classes.
data StateMemberDecl = StateDataDecl {
    -- | The name of the field.
    stateDataName   :: String,
    -- | The (optional) expression which specifies the default value.
    stateDataExpr   :: Maybe String,
    -- | The type of the field.
    stateDataType   :: String
} deriving Show

instance Pretty StateMemberDecl where
    pp (StateDataDecl{..}) =
        text stateDataName <+>
        optionally stateDataExpr (\e -> text "=" <+> text e) <+>
        text "::" <+>
        text stateDataType

-- | Represents a state class definition.
data StateDecl = StateDecl {
    -- | The class modifier, if any.
    stateMod      :: Maybe StateMod,
    -- | The name of the class.
    stateName     :: String,
    -- | The list of type parameters.
    stateParams   :: [String],
    -- | The name of the parent class, if any.
    stateParentN  :: Maybe String,
    -- | The type argument for the parent class.
    stateParentPs :: [String],
    -- | The parent class, once resolved.
    stateParent   :: Maybe StateDecl,
    -- | The field declarations.
    stateData     :: [StateMemberDecl],
    -- | The method signatures and definitions.
    stateBody     :: [Dec],
    -- | The method table for this class, once computed.
    stateMethods  :: MethodTable
}

instance Pretty StateDecl where
    pp (StateDecl{..}) =
        text "CLASS" <+> parens (text stateName) <> colon $$ nest 4 (vcat [
            optionally stateParentN (\p -> text "PARENT:" <+> text p <+> hsep (map text stateParentPs)),
            optionally stateParent (\_ -> text "PARENT RESOLVED"),
            optionally stateMod (\m -> text "MODIFIER:" <+> text (show m)),
            text "PARAMS:" <+> hsep (map text stateParams),
            text "DATA" <> colon $$ nest 4 (vcat (map pp stateData)),
            text "BODY (TH AST)" <> colon $$ nest 4 (vcat (map (text . Ppr.pprint) stateBody)),
            text "METHOD TABLE" <> colon $$ nest 4 (ppMethodTable stateMethods)
        ])

instance Show StateDecl where
    show = render . pp

--------------------------------------------------------------------------------

-- | `isBaseClass` @class determines whether @class is a base class.
isBaseClass :: StateDecl -> Bool
isBaseClass (StateDecl { stateParentN = Nothing } ) = True
isBaseClass _ = False

-- | `isAbstractClass` @class determines whether @class is abstract.
isAbstractClass :: StateDecl -> Bool
isAbstractClass (StateDecl { stateMod = Just Abstract }) = True
isAbstractClass _ = False

-- | `isFinalClass` @class determines whether @class is final.
isFinalClass :: StateDecl -> Bool
isFinalClass (StateDecl { stateMod = Just Final }) = True
isFinalClass _ = False

-- | `declByParent' @name @class determines whether a method named @name
-- was declared by a parent of @class.
declByParent :: Name -> StateDecl -> Bool
declByParent _ (StateDecl { stateParent = Nothing })  = False
declByParent n (StateDecl { stateParent = (Just p) }) =
    M.member (nameBase n) (methods $ stateMethods p) || declByParent n p

-- | `ctrsForClass` @dec returns a list of object states for the state class
--   described by @dec.
ctrsForClass :: StateDecl -> [StateObjCtr]
ctrsForClass (StateDecl { stateParentN = p, stateMod = m }) = case p of
    Nothing -> case m of
        Nothing         -> [DataCtr, StartCtr]
        (Just Abstract) -> [StartCtr]
        (Just Final)    -> [DataCtr]
    (Just _) -> case m of
        Nothing         -> [DataCtr, StartCtr, MiddleCtr, EndCtr]
        (Just Abstract) -> [StartCtr, MiddleCtr]
        (Just Final)    -> [DataCtr, EndCtr]
