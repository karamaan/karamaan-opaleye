-- | Generate ShowConstant instances for newtypes.
module Karamaan.Opaleye.ShowConstant.TH
  ( makeShowConstantInstance
  , ShowConstant (showConstant)
  , showThrough
  ) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Language.Haskell.TH
import Karamaan.Opaleye.ShowConstant (ShowConstant (..), showThrough)

-- ^^ This can be extended to allow type variables and work for normal
-- data types that have the same structure as newtypes.
makeShowConstantInstance :: Name -> Q [Dec]
makeShowConstantInstance = return . either fail (:[]) <=< f <=< reify
  where
    f :: Info -> Q (Either String Dec)
    f i = case i of
      TyConI (NewtypeD _ctx tyName _tyVars@[] con _names) ->
        case getConName con of
          Left err      -> return $ Left err
          Right conName -> Right <$> showConstantInstance tyName conName
      TyConI NewtypeD{} -> return $ Left "Type variables aren't allowed"
      _  -> return $ Left "Must be a newtype"

getConName :: Con -> Either String Name
getConName c = case c of
  NormalC conName _ -> Right conName
  RecC conName _ -> Right conName
  _ -> Left "Only normal and record constructors are allowed"

showConstantInstance :: Name -> Name -> Q Dec
showConstantInstance tyName conName = do
   x      <- newName "x"
   let ty  = ConT (mkName "ShowConstant") `AppT` (ConT tyName) `AppT` (ConT tyName)
   let e   = VarE (mkName "showThrough") `AppE` LamE [ConP conName [VarP x]] (VarE x)
   let imp = FunD (mkName "showConstant") [Clause [] (NormalB e) []]
   return $ InstanceD [] ty [imp]
