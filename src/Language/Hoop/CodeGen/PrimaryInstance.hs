module Language.Hoop.CodeGen.PrimaryInstance (
    genPrimaryInstance,
    genIdentityInstance,
    genParentalInstance
) where

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Hoop.StateEnv
import Language.Hoop.StateDecl
import Language.Hoop.CodeGen.Shared (renameParent, appN)
import Language.Hoop.CodeGen.SharedInstance
import Language.Hoop.CodeGen.Interop (parseType)

-- | Gets the name of the base monad. This is Identity for base classes
-- or the parent class's monad type for subclasses
getBaseMonad :: Maybe String -> [String] -> Type
getBaseMonad Nothing  _  = ConT $ mkName "Identity"
getBaseMonad (Just p) vs = appN (renameParent (\n -> n ++ "M") $ parseType p) vs

-- | Generates the primary instance for a state class. This is the CLike
-- instance with CObj CSt PMonad
genPrimaryInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec
genPrimaryInstance env cls decs decl@(StateDecl {
    stateName    = name,
    stateParams  = vars,
    stateData    = ds,
    stateParentN  = mp,
    stateParentPs = ps,
    stateMethods = methods
}) = do
    let
        cxt = []
        -- the name of the type class (CLike)
        cn  = mkName $ name ++ "Like"
        -- the name of the object type (C)
        on  = mkName name
        -- the name of the state type (CState)
        sn  = mkName $ name ++ "State"
        -- the name of the parent class's monad (PMonad)
        bt  = getBaseMonad mp ps
        -- CLike C CState PMonad tvars
        ty  = foldl AppT (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) (map (VarT . mkName) vars)
        -- a type function which returns the state type
        -- CSt C = CState
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
    -- generate the invoke method body
    -- invk <- genInvokeDef name
    -- generate the field definitions
    mods <- genFields decl decl PrimaryInst
    -- generate the methods
    ms   <- genMethods PrimaryInst decl decl methods name
    -- return the type class instance
    return $ InstanceD Nothing cxt ty ([fam {-,invk -}] ++ mods ++ ms)

-- | Generates the identity instance for a state class. This is the CLike
-- instance with CObj CSt Identity
genIdentityInstance :: StateEnv -> Dec -> [Dec] -> StateDecl -> Q Dec
genIdentityInstance env cls decs decl@(StateDecl {
    stateName    = name,
    stateParams  = vars,
    stateData    = ds,
    stateParentN  = mp,
    stateMethods = methods
}) = do
    let
        cxt = []
        cn  = mkName $ name ++ "Like"
        on  = mkName name
        sn  = mkName $ name ++ "State"
        bt  = ConT $ mkName "Identity"
        ty  = foldl AppT (AppT (AppT (AppT (ConT cn) (ConT on)) (ConT sn)) bt) (map (VarT . mkName) vars)
        fam = TySynInstD (mkName $ name ++ "St") $ TySynEqn [ConT on] (ConT sn)
        -- generate the invoke method body
    -- invk <- genInvokeDef name
    -- generate the field definitions
    fs   <- genFields decl decl IdentityInst
    -- generate the methods
    ms   <- genMethods IdentityInst decl decl methods name
    -- return the type class instance
    return $ InstanceD Nothing cxt ty ([fam {- ,invk -}] ++ fs ++ ms)

-- | Generates the parental instances for a state class. The first argument to
-- this function is the state class for which these parental instances are
-- being generated. The second argument is the ancestor for which the instances
-- are currently being generated. 
genParentalInstance :: StateDecl -> StateDecl -> Q [Dec]
genParentalInstance sub parent = trace ("PARENTAL INSTANCE (secondary+identity) OF " ++ stateName parent ++ " for " ++ stateName sub) $ do
    let
        cxt = []
        cn  = mkName $ (stateName parent) ++ "Like"
        on  = mkName (stateName sub)
        sn  = mkName $ (stateName sub) ++ "State"
        bt  = getBaseMonad (stateParentN sub) (stateParentPs sub)
        -- TODO: not sure if the parameters should be from the parent or inferred from the parent type?
        ps  = map (VarT . mkName) (stateParams parent)
        ty  = foldl AppT (ConT cn) ([ConT on, ConT sn, bt] ++ ps)
        idty = foldl AppT (ConT cn) ([ConT on, ConT sn, ConT $ mkName "Identity"] ++ ps)
    fs <- genFields parent sub SecondaryInst
    ms <- genMethods SecondaryInst parent sub (stateMethods sub) (stateName parent)
    ifs <- genFields parent sub IdentityInst
    ims <- genMethods IdentityInst parent sub (stateMethods sub) (stateName parent)
    rs <- case stateParent parent of
        Nothing  -> return []
        (Just p) -> genParentalInstance sub p
    return $ [ InstanceD Nothing cxt ty (fs ++ ms)
             , InstanceD Nothing cxt idty (ifs ++ ims)
             ] ++ rs
