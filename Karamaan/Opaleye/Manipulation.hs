{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Karamaan.Opaleye.Manipulation where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (Scope, ExprArr, Expr, runExprArr'',
                                 runExprArrStartEmpty, scopeOfWire,
                                 unsafeScopeLookup, runExprArrStart)
import Karamaan.Opaleye.QueryColspec (MWriter(Writer))
import qualified Karamaan.Opaleye.HaskellDB as H
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
import qualified Database.HaskellDB.Sql.Generate as G
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.Sql.Print (ppDelete, ppInsert, ppUpdate)
import qualified Database.HaskellDB.Sql as S
import Control.Arrow ((&&&))
import Karamaan.Opaleye.Table (Table(Table))
import Data.Profunctor.Product.Default (Default, def)
import Karamaan.Plankton ((.:))
import Karamaan.Opaleye.Values ((.:.))
import Data.Function (on)
import qualified Database.PostgreSQL.Simple as SQL
import Data.String (fromString)
import Data.Int (Int64)

-- A 'TableExprRunner wires wires'' is used to connect a 'Table wires'
-- to an 'ExprArr wires' o'.  In current usage 'o' is only ever 'Wire
-- Bool' but I guess it could be anything.  This is used to
-- essentially "apply an expression to a table".
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
data TableExprRunner wires wires' = TableExprRunner (MWriter Scope wires)
                                                    (wires -> wires')

-- A 'TableMaybeWrapper' is used to convert the 'Wire a's appearing in
-- the columns of a 'Table' to 'Wire (Maybe a)'s, so that they can be
-- matched with the 'Wire (Maybe a)'s occuring in the 'ExprArr' that is
-- used to perform an update or an insert.
newtype TableMaybeWrapper wires maybewires = TableMaybeWrapper
                                             (wires -> maybewires)

newtype MWriter2 m a = MWriter2 (a -> a -> m)

-- An 'Assocer' is used to associate the columns of a 'Table' with the
-- columns in an 'ExprArr' so the 'ExprArr' can be used to update or
-- insert into the 'Table'.
newtype Assocer a = Assocer (MWriter2 (Scope -> [(String, PrimExpr)]) a)

-- Like Assocer but just used to get the 'PrimExpr's out of an 'Expr'.
newtype AssocerE a = AssocerE (MWriter (Scope -> [PrimExpr]) a)

-- Very boring instance definitions.  In principle these could be
-- derived in the same way as Functor, Foldable and Traversable, but
-- neither Haskell in general nor GHC in particular support that
-- currently.
--
-- There's no "choice" in these instances.  We have to provide the
-- only thing that works, except for the Applicative instances where
-- there are two ways of ordering the actions (although there's only
-- really one /natural/ ordering).
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
  contramap f (MWriter2 w) = MWriter2 (w `on` f)

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

instance Monoid (AssocerE a) where
  mempty = AssocerE mempty
  AssocerE w `mappend` AssocerE w' = AssocerE (w <> w')

instance Contravariant AssocerE where
  contramap f (AssocerE w) = AssocerE (contramap f w)

instance ProductContravariant AssocerE where
  point = defaultPoint
  (***<) = defaultContravariantProduct
-- End of very boring instance definitions

-- arrange* do the meat of the computation
arrangeDelete :: TableExprRunner wires wires' -> Table wires
                 -> ExprArr wires' (Wire Bool)
                 -> SqlDelete
arrangeDelete tableExprRunner
              (Table tableName tableCols)
              conditionExpr
  = sqlDelete defaultSqlGenerator tableName [condition]
  where condition = runExprArr'' conditionExpr colsAndScope'
        colsAndScope' = colsAndScope tableExprRunner tableCols

colsAndScope :: TableExprRunner wires wires' -> wires -> (wires', Scope)
colsAndScope (TableExprRunner (Writer makeScope) adaptCols)
  = adaptCols &&& makeScope

arrangeInsert :: Assocer maybewires -> TableMaybeWrapper wires maybewires
                 -> Table wires -> Expr maybewires
                 -> SqlInsert
arrangeInsert assocer
              (TableMaybeWrapper maybeWrapper)
              (Table tableName tableCols)
              insertExpr
  = sqlInsert defaultSqlGenerator tableName assocs
    where tableMaybeCols = maybeWrapper tableCols
          assocs = primExprsOfAssocer assocer tableMaybeCols
                                      (runExprArrStartEmpty insertExpr ())

arrangeInsertReturning :: Assocer maybewires
                       -> TableMaybeWrapper wires maybewires
                       -> TableExprRunner wires wires' -> AssocerE returning
                       -> Table wires -> Expr maybewires
                       -> ExprArr wires' returning
                       -> H.SqlInsertReturning
arrangeInsertReturning ass tmr ter assr table e ea =
  H.SqlInsertReturning (arrangeInsert ass tmr table e) returnSqlExprs
  where colsAndScope' = colsAndScope ter tableCols
        Table _ tableCols = table
        returnPrimExprs :: [PrimExpr]
        returnPrimExprs = primExprsOfAssocerE assr
                                          (runExprArrStart ea colsAndScope')
        returnSqlExprs :: [S.SqlExpr]
        returnSqlExprs = map (G.sqlExpr defaultSqlGenerator) returnPrimExprs

primExprsOfAssocer :: Assocer t -> t -> (t, Scope, z) -> [(String, PrimExpr)]
primExprsOfAssocer (Assocer (MWriter2 assocer)) t (cols, scope, _)
  = assocer t cols scope

primExprsOfAssocerE :: AssocerE t -> (t, Scope, z) -> [PrimExpr]
primExprsOfAssocerE (AssocerE (Writer assocerE)) (cols, scope, _)
  = assocerE cols scope

arrangeUpdate :: TableExprRunner wires wires' -> Assocer maybewires
              -> TableMaybeWrapper wires maybewires
              -> Table wires -> ExprArr wires' maybewires
              -> ExprArr wires' (Wire Bool)
              -> SqlUpdate
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

-- arrange*Def pass the default typeclass instances in automatically,
-- to reduce boilerplate
arrangeDeleteDef :: Default TableExprRunner wires wires' =>
                    Table wires -> ExprArr wires' (Wire Bool) -> SqlDelete
arrangeDeleteDef = arrangeDelete def

arrangeInsertDef :: (Default (PPOfContravariant Assocer) maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires)
                    => Table wires -> Expr maybewires -> SqlInsert
arrangeInsertDef = arrangeInsert def' def
  where def' = unPPOfContravariant def

arrangeInsertReturningDef :: (Default (PPOfContravariant Assocer)
                                      maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires,
                     Default TableExprRunner wires wires',
                     Default (PPOfContravariant AssocerE) r r)
                    => Table wires -> Expr maybewires -> ExprArr wires' r
                             -> H.SqlInsertReturning
arrangeInsertReturningDef = arrangeInsertReturning def' def def def''
  where def' = unPPOfContravariant def
        def'' = unPPOfContravariant def
        -- ^^ TODO: really need a typeclass polymorphic version of these!

arrangeUpdateDef :: (Default TableExprRunner wires wires',
                     Default (PPOfContravariant Assocer) maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires) =>
                    Table wires -> ExprArr wires' maybewires
                    -> ExprArr wires' (Wire Bool)
                    -> SqlUpdate
arrangeUpdateDef = arrangeUpdate def def' def
  where def' = unPPOfContravariant def

instance Default TableExprRunner (Wire a) (Wire a) where
  def = TableExprRunner (Writer scopeOfWire) id

instance Default TableMaybeWrapper (Wire a) (Maybe (Wire a)) where
  def = TableMaybeWrapper Just

instance Default (PPOfContravariant Assocer) (Maybe (Wire a)) (Maybe (Wire a)) where
  def = (PPOfContravariant . Assocer . MWriter2) assocerWireMaybe

instance Default (PPOfContravariant AssocerE) (Wire a) (Wire a) where
  def = (PPOfContravariant . AssocerE . Writer) assocerEWire

-- 'Nothing' entries correspond to supplying no value in the SQL, so
-- for INSERTs the default value will be used, and for UPDATEs the
-- field will be left unaltered.
assocerWireMaybe :: Maybe (Wire a) -> Maybe (Wire a) -> Scope
                 -> [(String, PrimExpr)]
assocerWireMaybe w w' = maybe [] return . liftA3 assocerWire w w' . pure

assocerWire :: Wire a -> Wire a -> Scope -> (String, PrimExpr)
assocerWire (Wire s) w scope = (s, unsafeScopeLookup w scope)

assocerEWire :: Wire a -> Scope -> [PrimExpr]
assocerEWire = return .: unsafeScopeLookup

arrangeDeleteSqlDef :: Default TableExprRunner wires wires' =>
                    Table wires -> ExprArr wires' (Wire Bool) -> String
arrangeDeleteSqlDef  = show . ppDelete .: arrangeDeleteDef

arrangeInsertSqlDef :: (Default (PPOfContravariant Assocer) wires' wires',
                     Default TableMaybeWrapper wires wires')
                    => Table wires -> Expr wires' -> String
arrangeInsertSqlDef = show . ppInsert .: arrangeInsertDef

arrangeInsertReturningSqlDef :: (Default (PPOfContravariant Assocer)
                                         maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires,
                     Default TableExprRunner wires wires',
                     Default (PPOfContravariant AssocerE) r r)
                    => Table wires -> Expr maybewires -> ExprArr wires' r
                             -> String
arrangeInsertReturningSqlDef = show . H.ppInsertReturning
                               .:. arrangeInsertReturningDef

arrangeUpdateSqlDef :: (Default TableExprRunner wires wires',
                     Default (PPOfContravariant Assocer) maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires) =>
                    Table wires -> ExprArr wires' maybewires
                    -> ExprArr wires' (Wire Bool)
                    -> String
arrangeUpdateSqlDef = (show . ppUpdate) .:. arrangeUpdateDef

executeDeleteConnDef :: Default TableExprRunner wires wires' =>
                    SQL.Connection ->
                    Table wires -> ExprArr wires' (Wire Bool) -> IO Int64
executeDeleteConnDef conn =
  SQL.execute_ conn . fromString .: arrangeDeleteSqlDef

executeInsertConnDef :: (Default (PPOfContravariant Assocer)
                                 maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires)
                    => SQL.Connection -> Table wires -> Expr maybewires
                     -> IO Int64
executeInsertConnDef conn =
  SQL.execute_ conn . fromString .: arrangeInsertSqlDef

executeUpdateConnDef :: (Default TableExprRunner wires wires',
                     Default (PPOfContravariant Assocer) maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires) =>
                    SQL.Connection ->
                    Table wires -> ExprArr wires' maybewires
                    -> ExprArr wires' (Wire Bool)
                    -> IO Int64
executeUpdateConnDef conn =
  SQL.execute_ conn . fromString .:. arrangeUpdateSqlDef

executeDeleteDef :: Default TableExprRunner wires wires' =>
                    SQL.ConnectInfo ->
                    Table wires -> ExprArr wires' (Wire Bool) -> IO Int64
executeDeleteDef connectInfo t e = do
  conn <- SQL.connect connectInfo
  executeDeleteConnDef conn t e

executeInsertDef :: (Default (PPOfContravariant Assocer) maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires)
                    => SQL.ConnectInfo -> Table wires -> Expr maybewires
                    -> IO Int64
executeInsertDef connectInfo t e = do
  conn <- SQL.connect connectInfo
  executeInsertConnDef conn t e

executeUpdateDef :: (Default TableExprRunner wires wires',
                     Default (PPOfContravariant Assocer) maybewires maybewires,
                     Default TableMaybeWrapper wires maybewires) =>
                    SQL.ConnectInfo ->
                    Table wires -> ExprArr wires' maybewires
                    -> ExprArr wires' (Wire Bool)
                    -> IO Int64
executeUpdateDef connectInfo t e e' = do
  conn <- SQL.connect connectInfo
  executeUpdateConnDef conn t e e'
