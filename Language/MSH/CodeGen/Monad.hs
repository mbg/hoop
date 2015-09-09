module Language.MSH.CodeGen.Monad (
    genStateType
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop

genStateType :: [TyVarBndr] -> StateDecl -> Q Dec 
genStateType tyvars (StateDecl { stateName = name, stateParams = vars, stateParentN = mp }) = do
    let
        -- unlike in the paper, the type synonym isn't just the name of the class
        tname = mkName $ name ++ "M"
        stype = appN (ConT (mkName $ name ++ "State")) vars
    case mp of
        Nothing  -> return $ TySynD tname tyvars (AppT (ConT (mkName "State")) stype)
        (Just p) -> do
            let
                ptype = parseType p 
                -- we want the monad, not the object
                fixpt t@(ConT _) = renameT (\n -> n ++ "M") t
                fixpt (AppT f a) = AppT (fixpt f) a 
            return $ TySynD tname tyvars (AppT (AppT (ConT (mkName "StateT")) stype) (fixpt ptype))
