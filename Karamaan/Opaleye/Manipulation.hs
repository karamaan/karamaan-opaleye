{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.Manipulation where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (Scope, ExprArr, Expr, runExprArr'',
                                 runExprArrStartEmpty, eq, scopeOfWire, plus,
                                 unsafeScopeLookup, constant, runExprArrStart)
import Karamaan.Opaleye.QueryColspec
import Database.HaskellDB.PrimQuery (PrimExpr)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                ProductContravariant, point, (***<),
                                defaultEmpty, defaultProfunctorProduct,
                                defaultPoint, defaultContravariantProduct,
                                PPOfContravariant(PPOfContravariant),
                                unPPOfContravariant)
import Data.Functor.Contravariant (Contravariant, contramap)
import Control.Applicative (Applicative, (<*>), pure, liftA3)
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Database.HaskellDB.Sql (SqlDelete, SqlInsert, SqlUpdate)
import Database.HaskellDB.Sql.Generate (sqlDelete, sqlInsert, sqlUpdate)
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.Sql.Print (ppDelete, ppInsert, ppUpdate)
import Control.Arrow ((&&&), (<<<), first, arr)
import Karamaan.Opaleye.Default (Default, def)

data Table a = Table String a

-- TODO: The MWriter will insert every table column into the scope
-- even if the projector components projects some columns away.  Is
-- this what we want?  It will probably be hard to do something
-- different without introducting another ProductProfunctor.
data TableExprRunner t e = TableExprRunner (MWriter Scope t) (t -> e)

newtype TableMaybeWrapper a b = TableMaybeWrapper (a -> b)

newtype MWriter2 m a = MWriter2 (a -> a -> m)

newtype Assocer a = Assocer (MWriter2 (Scope -> [(String, PrimExpr)]) a)

instance Functor (TableExprRunner a) where
  fmap f (TableExprRunner w ff) = TableExprRunner w (fmap f ff)

instance Applicative (TableExprRunner a) where
  -- TODO: Surely need a default way of doing 'contramap (const ()) point'
  pure x = TableExprRunner (contramap (const ()) point) (pure x)
  TableExprRunner w ff <*> TableExprRunner w' ff' =
    TableExprRunner (w <> w') (ff <*> ff')

instance Profunctor TableExprRunner where
  dimap f g (TableExprRunner w ff) = TableExprRunner (contramap f w) (d ff)
    where d = dimap f g

instance ProductProfunctor TableExprRunner where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

instance Functor (TableMaybeWrapper a) where
  fmap f (TableMaybeWrapper ff) = TableMaybeWrapper (fmap f ff)

instance Applicative (TableMaybeWrapper a) where
  pure = TableMaybeWrapper . pure
  TableMaybeWrapper ff <*> TableMaybeWrapper fx = TableMaybeWrapper (ff <*> fx)

instance Profunctor TableMaybeWrapper where
  dimap f g (TableMaybeWrapper ff) = TableMaybeWrapper (dimap f g ff)

instance ProductProfunctor TableMaybeWrapper where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

instance Monoid m => Monoid (MWriter2 m a) where
  mempty = MWriter2 (pure mempty)
  MWriter2 w `mappend` MWriter2 w' = MWriter2 (w <> w')

instance Contravariant (MWriter2 m) where
  contramap f (MWriter2 w) = MWriter2 (\a b -> w (f a) (f b))

instance Monoid m => ProductContravariant (MWriter2 m) where
  point = defaultPoint
  (***<) = defaultContravariantProduct

instance Monoid (Assocer a) where
  mempty = Assocer mempty
  Assocer w `mappend` Assocer w' = Assocer (w <> w')

instance Contravariant Assocer where
  contramap f (Assocer w) = Assocer (contramap f w)

instance ProductContravariant Assocer where
  point = defaultPoint
  (***<) = defaultContravariantProduct

arrangeDelete :: TableExprRunner t a -> Table t -> ExprArr a (Wire Bool)
                 -> SqlDelete
arrangeDelete tableExprRunner
              (Table tableName tableCols)
              conditionExpr
  = sqlDelete defaultSqlGenerator tableName [condition]
  where condition = runExprArr'' conditionExpr (colsAndScope' tableExprRunner tableCols)

colsAndScope' :: TableExprRunner t u -> t -> (u, Scope)
colsAndScope' (TableExprRunner (Writer makeScope) adaptCols)
  = adaptCols &&& makeScope

arrangeInsert :: Assocer t' -> TableMaybeWrapper t t' -> Table t -> Expr t'
                 -> SqlInsert
arrangeInsert assocer
              (TableMaybeWrapper maybeWrapper)
              (Table tableName tableCols)
              insertExpr
  = sqlInsert defaultSqlGenerator tableName assocs
    where tableMaybeCols = maybeWrapper tableCols
          assocs = primExprsOfAssocer assocer tableMaybeCols
                                      (runExprArrStartEmpty insertExpr ())

primExprsOfAssocer :: Assocer t -> t -> (t, Scope, t1) -> [(String, PrimExpr)]
primExprsOfAssocer (Assocer (MWriter2 assocer)) t (cols, scope, _)
  = assocer t cols scope

arrangeUpdate :: TableExprRunner t u -> Assocer t' -> TableMaybeWrapper t t'
              -> Table t -> ExprArr u t' -> ExprArr u (Wire Bool) -> SqlUpdate
arrangeUpdate tableExprRunner
              assocer
              (TableMaybeWrapper maybeWrapper)
              (Table tableName tableCols)
              updateExpr
              conditionExpr
  = sqlUpdate defaultSqlGenerator tableName [condition] assocs
  where tableMaybeCols = maybeWrapper tableCols
        colsAndScope = colsAndScope' tableExprRunner tableCols
        assocs = primExprsOfAssocer assocer tableMaybeCols
                                    (runExprArrStart updateExpr colsAndScope)
        condition = runExprArr'' conditionExpr colsAndScope

instance Default TableExprRunner (Wire a) (Wire a) where
  def = TableExprRunner (Writer scopeOfWire) id

instance Default TableMaybeWrapper (Wire a) (Maybe (Wire a)) where
  def = TableMaybeWrapper Just

instance Default (PPOfContravariant Assocer) (Maybe (Wire a)) (Maybe (Wire a)) where
  def = (PPOfContravariant . Assocer . MWriter2) assocerWire

assocerWire :: Maybe (Wire a) -> Maybe (Wire a) -> Scope -> [(String, PrimExpr)]
assocerWire w w' = maybe [] return . liftA3 assocerWire' w w' . pure

assocerWire' :: Wire a -> Wire a -> Scope -> (String, PrimExpr)
assocerWire' (Wire s) w scope = (s, unsafeScopeLookup w scope)

testDelete :: String
testDelete = show (ppDelete sqlDelete')
  where table :: Table ((Wire Int, Wire Int), Wire Int)
        table = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")
        condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = eq <<< first plus
        sqlDelete' = arrangeDelete def table condExpr

testInsert :: String
testInsert = show (ppInsert sqlInsert')
  where table :: Table ((Wire Int, Wire Int), Wire Int)
        table  = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")
        insertExpr :: Expr ((Maybe (Wire Int), Maybe (Wire Int)),
                            Maybe (Wire Int))
        insertExpr = ((arr Just <<< constant 1)
                      &&& (arr (const Nothing)))
                     &&& (arr Just <<< plus <<< (constant 5 &&& constant 6))
        sqlInsert' = arrangeInsert def' def table insertExpr
        def' = unPPOfContravariant def

testUpdate :: String
testUpdate = show (ppUpdate sqlUpdate')
  where table :: Table ((Wire Int, Wire Int), Wire Int)
        table  = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")
        updateExpr :: ExprArr ((Wire Int, Wire Int),
                               Wire Int)
                              ((Maybe (Wire Int), Maybe (Wire Int)),
                               Maybe (Wire Int))
        updateExpr = (arr (const Nothing) &&& (arr Just <<< plus <<< arr fst))
                     &&& arr (const Nothing)
        condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = eq <<< arr ((fst . fst) &&& snd)

        sqlUpdate' = arrangeUpdate def def' def table updateExpr condExpr
        def' = unPPOfContravariant def
