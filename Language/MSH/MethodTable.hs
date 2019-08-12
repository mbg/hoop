module Language.MSH.MethodTable where

--------------------------------------------------------------------------------

import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Ppr as Ppr
import Language.MSH.Pretty

--------------------------------------------------------------------------------

-- | Represents an entry in the method table.
data MethodEntry
    -- |
    = OverridenMethod Dec Dec
    -- | The method is inherited (possibly abstract) from its parent.
    | InheritedMethod Bool Dec (Maybe Dec)
    -- | This is the first place the method is defined (possibly abstract).
    | GenesisMethod Bool (Maybe Dec) (Maybe Dec)
    deriving Show

-- | `abstractEntry` @methodEntry@ determines whether @methodEntry@ is abstract.
abstractEntry :: MethodEntry -> Bool
abstractEntry (OverridenMethod _ _) = False
abstractEntry (InheritedMethod a _ _) = a
abstractEntry (GenesisMethod a _ _) = a

instance Pretty MethodEntry where
    pp (OverridenMethod dec def) = 
        text "[OVERRIDEN]" <+> 
        text (pprint dec) $+$
        text (pprint def)
    pp (InheritedMethod abs dec mdef) = 
        conditionally abs (text "[ABSTRACT]") <+>
        text "[INHERITED]" <+> 
        text (pprint dec) $+$
        optionally mdef (text . pprint)
    pp (GenesisMethod abs dec mdef) = 
        conditionally abs (text "[ABSTRACT]") <+> 
        optionally dec (text . pprint) $+$
        optionally mdef (text . pprint)

-- | Represents the method table, containing signatures and definitions.
data MethodTable = MkMethodTable {
    methods :: M.Map String MethodEntry
} deriving Show

-- | `isGenesisIn` @name table@ determines whether @name@ is originally
-- defined in @table@.
isGenesisIn :: String -> MethodTable -> Bool
isGenesisIn n tbl = case M.lookup n (methods tbl) of
    Just (GenesisMethod _ _ _) -> True
    _ -> False

-- | `ppMethodTable` @table@ pretty-prints @table@.
ppMethodTable :: MethodTable -> Doc
ppMethodTable (MkMethodTable methods) = 
    vcat (map (\(n,d) -> pp d) (M.toList methods))

-- | `emptyMethodTable` represents an empty method table.
emptyMethodTable :: MethodTable
emptyMethodTable = MkMethodTable M.empty

-- | `addMethodSig` @name entry table@ adds @entry@ for a method named @name@
-- to @table@.
addMethodSig :: Name -> Dec -> MethodTable -> MethodTable
addMethodSig name dec tbl =
    let
        ms = methods tbl
        nb = nameBase name
    in case M.lookup nb ms of
        Nothing -> tbl { 
            methods = M.insert nb (GenesisMethod True (Just dec) Nothing) ms 
        }
        Just (GenesisMethod _ dec _) -> 
            error $ "Multiple typings for " ++ nb ++ " in class"
        Just (InheritedMethod _ _ _) -> 
            error $ "Trying to add typing for inherited method: " ++ nb -- tbl { methodSigs = M.insert nb dec sigs }
        Just _                   -> error $ "Multiple typings for " ++ nb ++ " in class"

-- override :: MethodEntry -> MethodEntry
-- override (InheritedMethod _ d) = OverridenMethod d
-- override d = d

addOverride :: Name -> Dec -> MethodTable -> MethodTable 
addOverride name def tbl = 
    let
        nb = nameBase name 
    in case M.lookup nb (methods tbl) of 
        Nothing -> error $ "Cannot override " ++ nb 
        Just (InheritedMethod _ dec _) -> tbl {
            methods = M.insert nb (OverridenMethod dec def) (methods tbl)
        }
        _ -> error "Unexpected case in `addOverride`"

addGenesisDef :: Name -> Dec -> MethodTable -> MethodTable
addGenesisDef name def tbl = 
    let
        nb = nameBase name 
    in case M.lookup nb (methods tbl) of 
        Nothing -> tbl {
            methods = M.insert nb (GenesisMethod False Nothing (Just def)) (methods tbl)
        }
        Just (GenesisMethod _ dec mdef) -> case mdef of
            Just _ -> error $ "Multiple definitions for " ++ nb
            Nothing -> tbl {
                methods = M.insert nb (GenesisMethod False dec (Just def)) (methods tbl)
            }
        _ -> error "Unexpected case in `addGenesisDef`"

-- addMethodDef :: Name -> MethodEntry -> MethodTable -> MethodTable
-- addMethodDef name dec@(OverridenMethod d) tbl =
--     let
--         sigs = methodSigs tbl
--         defs = methodDefs tbl
--         nb   = nameBase name

--         success = case M.lookup nb sigs of
--             Nothing  -> tbl { methods = M.insert nb dec defs }
--             Just sig -> tbl { methods = M.insert nb dec defs,
--                               methods = M.insert nb (override sig) sigs }
--     in case M.lookup nb defs of
--         Nothing                    -> success
--         Just (InheritedMethod _ _) -> success
--         Just _                     -> 
--             error $ "Multiple definitions for " ++ nb ++ " in class"
-- addMethodDef name dec tbl =
--     let
--         defs = methods tbl
--         nb   = nameBase name
--     in case M.lookup nb defs of
--         Nothing                    -> tbl { methods = M.insert nb dec defs }
--         Just (InheritedMethod _ _) -> tbl { methods = M.insert nb dec defs }
--         Just _                     -> 
--             error $ "Multiple definitions for " ++ nb ++ " in class"



isImplemented :: Name -> MethodTable -> Bool
isImplemented n tbl = M.member (nameBase n) (methods tbl)

--------------------------------------------------------------------------------
