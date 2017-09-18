module Language.MSH.CodeGen.Monad (
    genStateType
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.MSH.StateDecl
import Language.MSH.CodeGen.Shared
import Language.MSH.CodeGen.Interop

import Debug.Trace

--stateName = name, stateParams = vars, stateParentN = mp
genStateType :: [TyVarBndr] -> StateDecl -> Q Dec
genStateType tyvars (StateDecl {..}) = do
    let
        -- unlike in the paper, the type synonym isn't just the name of the class
        tname = mkName $ stateName ++ "M"
        stype = appN (ConT (mkName $ stateName ++ "State")) stateParams
    case stateParentN of
        Nothing  -> return $
            TySynD tname tyvars (AppT (ConT (mkName "State")) stype)
        (Just p) -> do
            let
                ptype = appN (parseType p) stateParentPs
                -- we want the monad, not the object
                fixpt t@(ConT _) = renameT (\n -> n ++ "M") t
                fixpt (AppT f a) = AppT (fixpt f) a
            trace (show stateParentPs ++ show ptype) $ return $ TySynD tname tyvars (AppT (AppT (ConT (mkName "StateT")) stype) (fixpt ptype))
