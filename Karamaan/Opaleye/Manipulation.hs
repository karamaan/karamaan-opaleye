{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Karamaan.Opaleye.Manipulation where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (Scope, ExprArr, Expr, runExprArr'',
                                 runExprArrStartEmpty, scopeOfWire,
                                 unsafeScopeLookup, runExprArrStart)
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
import Control.Arrow ((&&&))
import Karamaan.Opaleye.Table (Table(Table))
import Karamaan.Opaleye.Default (Default, def)

-- A 'TableExprRunner t e' is used to connect a 'Table t' to an
-- 'ExprArr e o'.  In current usage 'o' is only ever 'Wire Bool' but I
-- guess it could be anything.  This is used to essentially "apply an
-- expression to a table".
--
-- TODO: could this actually be done in terms of
--
-- applyExprArrToTable :: TableExprRunner t e -> Table t -> ExprArr e o
--                        -> Expr o
--
-- That would make things simpler.
--
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
  pure = TableExprRunner mempty . pure
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
  mempty = MWriter2 mempty
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
  where condition = runExprArr'' conditionExpr colsAndScope'
        colsAndScope' = colsAndScope tableExprRunner tableCols

colsAndScope :: TableExprRunner t u -> t -> (u, Scope)
colsAndScope (TableExprRunner (Writer makeScope) adaptCols)
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

primExprsOfAssocer :: Assocer t -> t -> (t, Scope, z) -> [(String, PrimExpr)]
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
        colsAndScope' = colsAndScope tableExprRunner tableCols
        assocs = primExprsOfAssocer assocer tableMaybeCols
                                    (runExprArrStart updateExpr colsAndScope')
        condition = runExprArr'' conditionExpr colsAndScope'

arrangeDeleteDef :: Default TableExprRunner t a =>
                    Table t -> ExprArr a (Wire Bool) -> SqlDelete
arrangeDeleteDef = arrangeDelete def

arrangeInsertDef :: (Default (PPOfContravariant Assocer) t' t',
                     Default TableMaybeWrapper t t')
                    => Table t -> Expr t' -> SqlInsert
arrangeInsertDef = arrangeInsert def' def
  where def' = unPPOfContravariant def

arrangeUpdateDef :: (Default TableExprRunner t u,
                     Default (PPOfContravariant Assocer) t' t',
                     Default TableMaybeWrapper t t') =>
                    Table t -> ExprArr u t' -> ExprArr u (Wire Bool)
                    -> SqlUpdate
arrangeUpdateDef = arrangeUpdate def def' def
  where def' = unPPOfContravariant def

instance Default TableExprRunner (Wire a) (Wire a) where
  def = TableExprRunner (Writer scopeOfWire) id

instance Default TableMaybeWrapper (Wire a) (Maybe (Wire a)) where
  def = TableMaybeWrapper Just

instance Default (PPOfContravariant Assocer) (Maybe (Wire a)) (Maybe (Wire a)) where
  def = (PPOfContravariant . Assocer . MWriter2) assocerWireMaybe

assocerWireMaybe :: Maybe (Wire a) -> Maybe (Wire a) -> Scope
                 -> [(String, PrimExpr)]
assocerWireMaybe w w' = maybe [] return . liftA3 assocerWire w w' . pure

assocerWire :: Wire a -> Wire a -> Scope -> (String, PrimExpr)
assocerWire (Wire s) w scope = (s, unsafeScopeLookup w scope)
