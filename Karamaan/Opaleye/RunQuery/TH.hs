{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Karamaan.Opaleye.RunQuery.TH where

import Language.Haskell.TH (Dec(NewtypeD, DataD), Con(RecC, NormalC),
                            Type(ConT), Q, Exp(ConE), Name,
                            Info(TyConI), reify, TyVarBndr)
import Data.Profunctor.Product.TH (Error, varNameOfBinder)
import Control.Monad ((<=<), when)

import Karamaan.Opaleye.RunQuery (QueryRunner, fieldQueryRunnerF)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Nullable (Nullable)
import Data.Profunctor.Product.Default (Default, def)

-- Given something like
-- 
--     newtype StrategyId = StrategyId { fromStrategyId :: Int }
--
-- makes the Default QueryRunner instances
--
-- instance Default QueryRunner (Wire StrategyId) StrategyId where
--   def = fieldQueryRunnerF StrategyId
-- instance Default QueryRunner (Wire (Nullable StrategyId))
--                                    (Nullable StrategyId) where
--   def = fieldQueryRunnerF (fmap StrategyId)

makeWireQueryRunnerInstance :: Name -> Q [Dec]
makeWireQueryRunnerInstance = returnOrFail <=< r make <=< reify
  where r = (return .)
        returnOrFail (Right decs) = decs
        returnOrFail (Left errMsg) = fail errMsg
        make = makeWireQueryRunnerInstanceE

makeWireQueryRunnerInstanceE :: Info -> Either Error (Q [Dec])
makeWireQueryRunnerInstanceE info = do
  (tyName, tyVars, conName) <- newtypeDecStuffOfInfo info
  when ((not . null) tyVars) (Left "I can't handle newtypes with type parameters")
  -- TODO: ^^ this restriction could be removed if someone wants to put in the
  -- effort to generalise the instance construction
  let instanceDefinitions' = instanceDefinitions tyName conName

  return instanceDefinitions'
  
instanceDefinitions :: Name -> Name -> Q [Dec]
instanceDefinitions tyName conName = instanceDec where
  baseType = return (ConT tyName)
  baseCon = return (ConE conName)

  instanceDec = [d| instance Default QueryRunner (Wire $baseType)
                                                  $baseType where
                        def = fieldQueryRunnerF $baseCon

                    instance Default QueryRunner (Wire (Nullable $baseType))
                                                  (Maybe $baseType) where
                        def = fieldQueryRunnerF (fmap $baseCon) |]

newtypeDecStuffOfInfo :: Info -> Either Error (Name, [Name], Name)
newtypeDecStuffOfInfo (TyConI (NewtypeD _cxt tyName tyVars constructor _deriving)) =
  do
    conName <- newtypeConstructorName constructor
    let tyVars' = map varNameOfBinder tyVars
    return (tyName, tyVars', conName)
newtypeDecStuffOfInfo _ = Left "That doesn't look like a newtype declaration to me"

newtypeConstructorName :: Con -> Either Error Name
newtypeConstructorName (NormalC name _) = return name
newtypeConstructorName (RecC name _) = return name
newtypeConstructorName _ = Left "Unexpected non-newtype constructor"

makeWireQueryRunnerInstanceEnum :: Name -> Q [Dec]
makeWireQueryRunnerInstanceEnum = returnOrFail <=< r make <=< reify
  where r = (return .)
        returnOrFail (Right decs) = decs
        returnOrFail (Left errMsg) = fail errMsg
        make = makeWireQueryRunnerInstanceEnumE

makeWireQueryRunnerInstanceEnumE :: Info -> Either Error (Q [Dec])
makeWireQueryRunnerInstanceEnumE info = do
  (tyName, tyVars) <- dataDecStuffOfInfo info
  when ((not . null) tyVars) (Left "I can't handle data with type parameters")
  let instanceDefinitions' = instanceDefinitionsEnum tyName

  return instanceDefinitions'

instanceDefinitionsEnum :: Name -> Q [Dec]
instanceDefinitionsEnum tyName = instanceDec where
  baseType = return (ConT tyName)

  instanceDec = [d| instance Default QueryRunner (Wire $baseType)
                                                  $baseType where
                        def = fieldQueryRunnerF toEnum

                    instance Default QueryRunner (Wire (Nullable $baseType))
                                                  (Maybe $baseType) where
                        def = fieldQueryRunnerF (fmap toEnum) |]

dataDecStuffOfInfo :: Info -> Either Error (Name, [TyVarBndr])
dataDecStuffOfInfo (TyConI (DataD _cxt tyName tyVars _constructors _deriving)) =
  return (tyName, tyVars)
dataDecStuffOfInfo _ = Left "That doesn't look like a data declaration to me"
