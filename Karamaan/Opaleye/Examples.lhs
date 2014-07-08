> {-# LANGUAGE Arrows, FlexibleContexts #-}
> -- TODO: Get rid of FlexibleContexts if we ever move the definition of s
> --    elsewhere.
> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
>
> module Karamaan.Opaleye.Examples where
>
> import Prelude hiding (sum)
> import Karamaan.Opaleye.Unpackspec (Unpackspec)
> import Karamaan.Opaleye.Table (makeTableDef)
> import Karamaan.Opaleye.QueryArr (Query, QueryArr)
> import Karamaan.Opaleye.Nullable (Nullable)
> import Karamaan.Opaleye.Aggregate (aggregate, groupBy, sum, avg, count)
> import qualified Karamaan.Opaleye.Operators2 as Op2
> import qualified Karamaan.Opaleye.Predicates as P
> import qualified Karamaan.Opaleye.Operators.Numeric as N
> import qualified Karamaan.Opaleye.ExprArr as E
> import qualified Karamaan.Opaleye.LeftJoin as LJ
> import Karamaan.Opaleye.Wire (Wire)
> import Karamaan.Opaleye.SQL (showSqlForPostgresDefault)
> import Control.Category ((<<<))
> import Control.Arrow (arr, (&&&), returnA)
> import Data.Time.Calendar (Day)
>
> import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
> import Data.Profunctor.Product (PPOfContravariant, ProductProfunctor, p2, p5)
> import Data.Profunctor (dimap)
> import Data.Profunctor.Product.Default (Default, def)
> import qualified Database.PostgreSQL.Simple as SQL
> import Karamaan.Opaleye.RunQuery as RQ


Introduction
============

In this example file I'll give you a brief introduction to the Opaleye
relational query EDSL.  I'll show you how to define tables in Opaleye;
use them to generate selects, joins and filters; use the API of
Opaleye to make your queries more composable; and finally run the
queries on Postgres.

Opaleye uses HaskellDB's SQL generator.  You should really use the
latest version from https://github.com/m4dc4p/haskelldb because it
patches some important bugs present in 2.2.2.

Schema
======

A Query that returns the contents of a table is defined with
'makeTableDef'.  The syntax is simple.  You specify the types of the
columns and the names of the columns.  'Wire X' essentially means "a
column of type X".

There is some typeclass magic which matches the names of the columns
with a field in the query result type.  In Opaleye you never *have* to
use typeclasses.  All the magic that typeclasses do is also available
by explicitly passing in the "typeclass dictionary".  However, for
this example file we will always use the typeclass versions because
they are simpler to read.

For tuples, the typeclass instances are already defined, so you can
just go ahead and define your table specification.

> personTable :: Query (Wire String, Wire Int, Wire String)
> personTable = makeTableDef ("name", "age", "address") "personTable"

If we generate the SQL for this we see the following:

ghci> sh personTable
SELECT name as name_1,
       age as age_1,
       address as address_1
FROM personTable as T1

('sh' is just a conveniently named utility function for the purposes
of this example file)

Opaleye can use user defined types in queries.  It will save you a lot
of headaches if you define your typeclasses to be polymorphic in all
their fields.  If you want to use concrete types in particular places,
as you almost always will, you can use type synonyms.  For example:

> data Birthday' a b = Birthday { bdName :: a, bdDay :: b }
> type Birthday = Birthday' String Day
> type WireBirthday = Birthday' (Wire String) (Wire Day)

To get user defined types to work with the typeclass magic they must
have instances defined for them.  The instances are derivable with
Template Haskell.

> $(makeAdaptorAndInstance "pBirthday" ''Birthday')

Then we can use 'makeTableDef' to make a table that returns our record
type in exactly the same way as before.  This usage makes it clear why
the fields should be polymorphic.  The argument to 'makeTableDef' has
type 'Birthday' String String', whilst in the return value it is
'Birthday' (Wire String) (Wire Day)'.  In practice you don't need to
worry about this, just make all your fields polymorphic!

> birthdayTable :: Query WireBirthday
> birthdayTable = makeTableDef (Birthday { bdName = "name"
>                                        , bdDay = "birthday" })
>                     "birthdayTable"

ghci> sh birthdayTable
SELECT name as name_1,
       birthday as birthday_1
FROM birthdayTable as T1

Projection
==========

The type 'Query a' is a type synonym for 'QueryArr () a', and
'QueryArr' is an instance of 'Arrow'.  For the purposes of projection
you need to know that this means there is a combinator

    arr :: (a -> b) -> QueryArr a b

and a combinator

    <<< :: QueryArr b c -> QueryArr a b -> QueryArr a c

This allows us to use a function of type 'a -> b' to project columns
of type 'b' from a 'Query a'.  For example, if we want just the first
and second columns of 'personTable'

> nameAge :: Query (Wire String, Wire Int)
> nameAge = arr (\(x, y, _) -> (x, y)) <<< personTable

ghci> sh nameAge
SELECT name as name_1,
       age as age_1
FROM personTable as T1

Product
=======

You can take the cartesian product of two queries by using the arrow
'(&&&)' combinator.  Specialised to 'Query' the type is

    (&&&) :: Query a -> Query b -> Query (a, b)

For example, to take the product of the 'personTable' and the
'birthdayTable' we do

> personBirthdayProduct :: Query ((Wire String, Wire Int, Wire String),
>                                 WireBirthday)
> personBirthdayProduct = personTable &&& birthdayTable

ghci> sh personBirthdayProduct
SELECT name_1,
       age_1,
       address_1,
       name_2,
       birthday_2
FROM (SELECT name as name_1,
             age as age_1,
             address as address_1
      FROM personTable as T1) as T1,
     (SELECT name as name_2,
             birthday as birthday_2
      FROM birthdayTable as T1) as T2

Note that in 'personBirthdayProduct' we end up with a nested tuple.
If you prefer to have a flattened tuple, one way of flattening it
would be with the technique we learned above in "Projection".  Note
that in practice, however, you probably won't end up writing queries
in this more "low level" way which is somewhat fiddly, but use higher
level combinators.  Still, this is a useful example.

> personBirthdayProduct' :: Query (Wire String, Wire Int, Wire String,
>                                 WireBirthday)
> personBirthdayProduct' = arr (\((x, y, z), b) -> (x, y, z, b))
>                          <<< personBirthdayProduct

The generated SQL will be exactly the same as before.

There is a further way to do product and unflattening in one go, and
that is to use Arrow notation.  Arrow notation is a GHCi extension
which is *extremely* useful when writing Opaleye queries.  This is the
previous example rewritten with Arrow notation.

> personBirthdayProduct'' :: Query (Wire String, Wire Int, Wire String,
>                                 WireBirthday)
> personBirthdayProduct'' = proc () -> do
>   (x, y, z) <- personTable   -< ()
>   b         <- birthdayTable -< ()
>   returnA -< (x, y, z, b)

Again the same SQL is generated.

Arrow notation is much more convenient than Arrow combinators when
writing involved queries.  As a simple introduction we'll see how to
how to add two numeric fields.  This query returns all pairs of
people, and the sum of their ages.

> totalAge :: Query (Wire String, Wire String, Wire Int)
> totalAge = proc () -> do
>   (name1, age1, _) <- personTable -< ()
>   (name2, age2, _) <- personTable -< ()
>
>   sumAge <- N.plus -< (age1, age2)
>
>   returnA -< (name1, name2, sumAge)

ghci>
SELECT name_1,
       name_2,
       age_1 + age_2 as age_1_plus_age_2_3
FROM (SELECT name as name_1,
             age as age_1
      FROM personTable as T1) as T1,
     (SELECT name as name_2,
             age as age_2
      FROM personTable as T1) as T2

Join
====

In Opaleye you express a join by taking the product of two tables and
then restricting the result to the case where an equality holds
between two columns.  This approach is very general and extends not
just to joining based on equality of columns but on inequalities and
all sorts of other predicates.

If we want to join 'personTable' to 'birthdayTable' in order to look
up the birthday of every person we can do that with the following,
which is essentially a product followed by a restriction.

> personAndBirthday :: Query (Wire String, Wire Int, Wire String, Wire Day)
> personAndBirthday = proc () -> do
>   (name, age, address) <- personTable -< ()
>   birthday <- birthdayTable -< ()
>
>   P.restrict <<< Op2.eq -< (name, bdName birthday)
>
>   returnA -< (name, age, address, bdDay birthday)

ghci> sh personAndBirthday
SELECT name_1,
       age_1,
       address_1,
       birthday_2
FROM (SELECT name as name_1,
             age as age_1,
             address as address_1
      FROM personTable as T1) as T1,
     (SELECT name as name_2,
             birthday as birthday_2
      FROM birthdayTable as T1) as T2
WHERE (name_1 = name_2)

Composability
-------------

This query gives us our first opportunity for a glimpse at the
enormous composability that Opaleye offers.

The relationship that we are trying to express between a person's name
and their birthday is a process which "takes in" their name and "gives
out" their birthday.  'birthdayOfPerson' encodes this this directly in
Opaleye.  It "takes in" a name and returns all rows of 'birthdayTable'
which match that name (which, if the birthday table is defined
properly in our DBMS, should be only one row!).  Using Arrow notation
makes this informal description of the behaviour simple to implement.

> birthdayOfPerson :: QueryArr (Wire String) (Wire Day)
> birthdayOfPerson = proc name -> do
>   birthday <- birthdayTable -< ()
>
>   P.restrict <<< Op2.eq -< (name, bdName birthday)
>
>   returnA -< bdDay birthday

We can't generate "the SQL of" birthdayOfPerson.  Since it's not a
'Query' it doesn't have any SQL!  What we do with it is use it to
reimplement 'personAndBirthday'' in a more neatly-factored way.

> personAndBirthday' :: Query (Wire String, Wire Int, Wire String, Wire Day)
> personAndBirthday' = proc () -> do
>   (name, age, address) <- personTable -< ()
>   birthday <- birthdayOfPerson -< name
>
>   returnA -< (name, age, address, birthday)

ghci> sh personAndBirthday'
SELECT name_1,
       age_1,
       address_1,
       birthday_2
FROM (SELECT name as name_1,
             age as age_1,
             address as address_1
      FROM personTable as T1) as T1,
     (SELECT name as name_2,
             birthday as birthday_2
      FROM birthdayTable as T1) as T2
WHERE (name_1 = name_2)

Note that the generated SQL is exactly the same as before.  Pulling
out 'birthdayOfPerson' was merely a refactoring.  It didn't change
behaviour.

More joins
----------

Here's an example of restricting with an inequality condition.  This
query finds everyone whose age is less than 18.

> children :: Query (Wire String, Wire Int, Wire String)
> children = proc () -> do
>   row@(_, age, _) <- personTable -< ()
>   -- Note: having to pull out the 'constant 18' explicitly
>   -- is a bit messy.  A better syntax for this is an
>   -- active research project!
>   eighteen <- Op2.constant 18 -< ()
>   P.restrict <<< N.lt -< (age, eighteen)
>
>   returnA -< row

ghci> sh children
SELECT name as name_1,
       age as age_1,
       address as address_1
FROM personTable as T1
WHERE (age < 18)

Here's an example with more restrictions.  It returns everyone who is
not in their twenties, and lives at a specific address.

> notTwentiesAtAddress :: Query (Wire String, Wire Int, Wire String)
> notTwentiesAtAddress = proc () -> do
>   row@(_, age, address) <- personTable -< ()
>   twenty    <- Op2.constant 20 -< ()
>   thirty    <- Op2.constant 30 -< ()
>
>   ltTwenty  <- N.lt  -< (age, twenty)
>   gteThirty <- N.gte -< (age, thirty)
>
>   P.restrict <<< Op2.or -< (ltTwenty, gteThirty)
>
>   myAddress <- Op2.constant "1 My Street, My Town" -< ()
>
>   P.restrict <<< Op2.eq -< (address, myAddress)
>
>   returnA -< row

ghci> sh notTwentiesAtAddress
SELECT name as name_1,
       age as age_1,
       address as address_1
FROM personTable as T1
WHERE (address = '1 My Street, My Town') AND (age < 20 OR age >= 30)

More composability
------------------

We can factor out some parts of the 'notTwentiesAtAddress' query.  For
example we can pull out the check for being 'notTwenties' and the
check 'addressIs1MyStreet'.

> notTwenties :: QueryArr (Wire Int) (Wire Bool)
> notTwenties = proc age -> do
>   twenty <- Op2.constant 20 -< ()
>   thirty <- Op2.constant 30 -< ()
>   ltTwenty  <- N.lt  -< (age, twenty)
>   gteThirty <- N.gte -< (age, thirty)
>   Op2.or -< (ltTwenty, gteThirty)
>
> addressIs1MyStreet :: QueryArr (Wire String) (Wire Bool)
> addressIs1MyStreet = proc address -> do
>   myAddress <- Op2.constant "1 My Street, My Town" -< ()
>   Op2.eq -< (address, myAddress)
>
> notTwentiesAtAddress' :: Query (Wire String, Wire Int, Wire String)
> notTwentiesAtAddress' = proc () -> do
>   row@(_, age, address) <- personTable -< ()
>
>   P.restrict <<< notTwenties -< age
>   P.restrict <<< addressIs1MyStreet -< address
>
>   returnA -< row

The generated SQL is again exactly the same as before.

> sh :: Default (PPOfContravariant Unpackspec) a a
>       => Query a -> IO ()
> sh = putStrLn . showSqlForPostgresDefault

Aggregation
===========

Type safe aggregation is the jewel in the crown of Opaleye.  Both
HaskellDB and Esqueleto have aggregation implementations that allow
the application programmer to produce an invalid SQL query.  By
contrast, every Opaleye expression you can write generates well formed
SQL.  Of course there may be bugs in the implemenation, but the idea
is that there are no bugs in the API!

By way of example, suppose we have a widget table which contains the
style, color, location, quantity and radius of widgets.  We can model
this information with the following datatype.

> data Widget a b c d e = Widget' { style    :: a
>                                 , color    :: b
>                                 , location :: c
>                                 , quantity :: d
>                                 , radius   :: e }

For the purposes of this example the style, color and location will be
strings, but in practice you'll probably want to use an abstract data
type for them.

> widgetTable :: Query (Widget (Wire String) (Wire String) (Wire String)
>                              (Wire Int) (Wire Double))
> widgetTable = makeTableDef (Widget' { style    = "style"
>                                     , color    = "color"
>                                     , location = "location"
>                                     , quantity = "quantity"
>                                     , radius   = "radius" })
>                            "widgetTable"
>
> $(makeAdaptorAndInstance "pWidget" ''Widget)

Here we see the first explict use of our Template Haskell derived
code.  We use the 'pWidget' "adaptor" to specify how columns are
aggregated.  Note that this is yet another example of avoiding a
headache by keeping your datatype fully polymorphic, because the
'count' aggregator changes a 'Wire String' into a 'Wire Int'.

'aggregateWidgets' groups by the style and color of widgets,
calculating how many (possibly duplicated) locations there are, the
total number of such widgets and their average radius.

> aggregateWidgets :: Query (Widget (Wire String) (Wire String) (Wire Int)
>                              (Wire Int) (Wire Double))
> aggregateWidgets = aggregate (pWidget (Widget' { style    = groupBy
>                                                , color    = groupBy
>                                                , location = count
>                                                , quantity = sum
>                                                , radius   = avg }))
>                              widgetTable

The Opaleye corresponds closely to the generated SQL.

ghci> sh widgetTable
SELECT style as style_1_2,
       color as color_1_2,
       COUNT(location) as location_1_2,
       SUM(quantity) as quantity_1_2,
       AVG(radius) as radius_1_2
FROM widgetTable as T1
GROUP BY style,
         color

Outer join
==========

Opaleye supports left joins.  (Full outer joins will be added when I
get round to it!  File an issue[1] if you need them).

> type WireNullableBirthday = Birthday' (Wire (Nullable String))
>                                       (Wire (Nullable Day))

> personBirthdayLeftJoin :: Query ((Wire String, Wire Int, Wire String),
>                                  WireNullableBirthday)
> personBirthdayLeftJoin = LJ.leftJoin' personTable birthdayTable eqName
>     where eqName :: E.ExprArr ((Wire String, Wire Int, Wire String),
>                                WireBirthday) (Wire Bool)
>           eqName = proc ((name, _, _), birthdayRow) -> do
>             E.eq -< (name, bdName birthdayRow)

ghci> sh personBirthdayLeftJoin
SELECT name_1,
       age_1,
       address_1,
       name_2,
       birthday_2
FROM ((SELECT name as name_1,
       age as age_1,
       address as address_1
FROM personTable as T1) AS T1 LEFT OUTER JOIN (SELECT name as name_2,
       birthday as birthday_2
FROM birthdayTable as T1) AS T2 ON (name_1) = (name_2)) as T1

Because SQL turns non-nullable columns in the right input query into
nullable columns we have to change the type of the output to have
nullable columns.  There is a multiparameter typeclass to make the
type change and in order to get the type checking to work
satisfactorily you need to provide type signatures for both the input
and the output.

Running queries on Postgres
===========================

Opaleye provides simple facilities for running queries on Postgres.
Other DBMSes are not forbidden, but have just not been tried!

For example we can run the 'notTwentiesAtAddress' query as below.
Note that this particular formulation uses typeclasses so please put
type signatures on everything in sight to minimize the number of
confusing error messages!

> notTwentiesQuery :: SQL.ConnectInfo -> IO [(String, Int, String)]
> notTwentiesQuery connectInfo = RQ.runQueryDefaultConnectInfo connectInfo
>                                                 notTwentiesAtAddress

Note that nullable columns are indicated with the Nullable type
constructor, and these are converted to Maybe when executed.  If we
have a table with a nullable column like the following

> widgets :: Query (Wire String, Wire (Nullable Int))
> widgets = makeTableDef ("widget_location", "widget_quantity") "widgets_table"

then when we run it the nullable columns turns into a column of Maybes

> widgetsQuery :: SQL.ConnectInfo -> IO [(String, Maybe Int)]
> widgetsQuery connectInfo = RQ.runQueryDefaultConnectInfo connectInfo widgets

Conclusion
==========

There ends the Opaleye introductions module.  Please send me your questions!

[1] https://github.com/karamaan/karamaan-opaleye/issues
