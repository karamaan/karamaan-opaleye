# Notes on technical design

## How to think of query arrows

A `QueryArr a b` logically represents a unit of functionality which
receives rows of type `a` and for each such row yields zero or more
rows of type `b`.  A good intuition for `QueryArr` is to think of
`QueryArr a b` as similar to `a -> [b]`.  For example, the query arrow

    foo :: QueryArr (Wire FirstName) (Wire Surname, Wire Age)

might yield the surnames and ages of every person with the given first
name.  If you precompose `foo` with `names :: Query (Wire FirstName)`
then you receive a `Query (Wire Surname, Wire Age)` which is a
collection of rows of `(surname, age)` for every `name` in `names`.

In an informal lambda-style notation, `foo` might be expressed as

    \names -> SELECT people.surname, people.age
              FROM people, names
              WHERE people.name = names.name

In general for a query arrow `foo :: QueryArr a b` the underlying
query looks something like

    \a -> SELECT ... columns of b ...
          FROM ... some tables ...
          WHERE ... some conditions ...

In a `QueryArr () b` the argument to the query arrow is just `()`.  In
this case the value is a collection of rows with columns of type `b`.
For intuition compare this with `() -> [b]`.

## Unions of query arrows

Opaleye allows you to take unions of values of type `QueryArr () b`.
This corresponds directly to SQL's `UNION`, and the Haskell list
analogue is `a ++ b`.  (There is a caveat here.  `++` is closer to
`UNION ALL` since it does not remove duplicates, but the difference
between the two is irrelevant for the purposes of this document.)

If we have two queries `foo` and `bar` of type `QueryArr () b` then the
lambda-style code for their union can be written

    SELECT ... columns of type b ...
    FROM ... the tables of foo ...
    WHERE ... some conditions involving foo ...
    UNION
    SELECT ... columns of type b ...
    FROM ... the tables of bar ...
    WHERE ... some conditions involving bar ...

If instead `foo` and `bar` have the type `QueryArr a b` -- i.e. they both
take an argument -- then using our Haskell list analogy we want the
resultant query to behave like `\a -> foo a ++ bar a` i.e.

    \a -> SELECT ... columns of type b ...
          FROM .. the tables of foo and a ...
          WHERE ... conditions involving foo and a ...
          UNION
          SELECT ... columns of type b ...
          FROM .. the tables of bar and a ...
          WHERE ... conditions involving bar and a ...
          
If we precompose this with `input :: Query a` then we can use an SQL
common table expression for `input` so that it is only evaluated once.
Thus it seems we can express these unions of query arrows efficiently
in SQL.

However, it is harder to do this *generically* in SQL.  For example,
what happens if we want to do

    proc a -> do
        b <- foo `union` bar -< a
        returnA (b, a)

in Opaleye?  It may help to know that this is the equivalent of

    \a -> (,) <$> (foo a ++ bar a) <*> [a]

The input columns must also appear in the output so this means that we
must be able to generate SQL similar to this

    \a -> SELECT ... columns of type b and a ...
          FROM .. the tables of foo and a ...
          WHERE ... conditions involving foo and a ...
          UNION
          SELECT ... columns of type b and a ...
          FROM .. the tables of bar and a ...
          WHERE ... conditions involving bar and a ...

That is, we must augment our earlier query with the input columns as well.

If we want to do *two* unions things become even more complicated.
Suppose we have `baz` and `quux` of type `QueryArr a c`.  Consider

    proc a -> do
        b <- foo `union` bar  -< a
        c <- baz `union` quux -< a
        returnA (b, c)

The Haskell list equivalent is

    \a -> (,) <$> (foo a ++ bar a) <*> (baz a ++ quux a)

This time the generated SQL must be more complicated, containing
iterated unions.  The structure of the generated SQL cannot mimic the
structure of the above Haskell expression because SQL is not flexible
enough to express the original directly.  Instead we rewrite it to the
following equivalent

    \a ->    ((,) <$> (foo a ++ bar a) <*> baz a)
          ++ ((,) <$> (foo a ++ bar a) <*> quux a)

leading to SQL of

    \a -> SELECT ... columns of type (b, c) ...
          FROM
          ... tables of baz ...,
              SELECT ... columns of type b and a ...
              FROM .. the tables of foo and a ...
              WHERE ... conditions involving foo and a ...
              UNION
              SELECT ... columns of type b and a ...
              FROM .. the tables of bar and a ...
              WHERE ... conditions involving bar and a ...
          WHERE ... conditions involving baz and a ...
          UNION
          SELECT ... columns of type (b, c) ...
          FROM
          ... tables of quux ...,
              SELECT ... columns of type b and a ...
              FROM .. the tables of foo and a ...
              WHERE ... conditions involving foo and a ...
              UNION
              SELECT ... columns of type b and a ...
              FROM .. the tables of bar and a ...
              WHERE ... conditions involving bar and a ...
          WHERE ... conditions involving quux and a ...

This poses a number of problems.  Firstly, the code generation is
difficult because the AST must keep track of all intermediate variables
in case they are ever used again in the expression.  It will be hard,
if not impossible, to get HaskellDB's AST to support this
functionality.  Secondly, it is unclear how well such queries will
perform and whether or not performance will predictible to the
programmer.  There is an arbitrary choice involved which may have a
performance impact.  We could have chosen the SQL to mimic this
equivalent alternative and it may be difficult in general to tell
which will perform better.

    \a ->    ((,) <$> foo a <*> (baz a ++ quux a))
          ++ ((,) <$> bar a <*> (baz a ++ quux a))

In conclusion, taking unions of query arrows is a sensible, and indeed
very useful, thing for applications programmers to be able to do.
However, SQL does not make it easy to support this kind of
functionality and it is unclear what the performance implications will
be.  This functionality will be left as something to explore in the
much longer term.

## Expression arrows

Currently the only way to create expressions is via "expression
arrows".  This is somewhat cumbersome.  Fortunately expression arrows
are "pure", meaning that amongst other nice properties,

    a  <- aArr -< aIn
    a' <- aArr -< aIn
    f -< (a, a')

is the same as

    a <- aArr -< aIn
    f -< (a, a)

and

    a <- aArr -< aIn
    b <- bArr -< bIn
    returnA -< (a, b)

is the same as

    b <- bArr -< bIn
    a <- aArr -< aIn
    returnA -< (a, b)

Having got a good sense of how expression arrows work I think they
could be made genuinely pure.  That is to say, instead of having

    plus :: ExprArr (Wire Int, Wire Int) (Wire Int)

we could have simply

    plus :: Wire Int -> Wire Int -> Wire Int

I am not aware of any way this could lead to invalid queries, but at
the same time I am not certain that it cannot.  I will hold of on
making this change for now, as it would be very hard to reverse, but
it is worth keeping in mind.

The change that would be made is simple.  `Wire a` would no longer
contain a string referring to a column.  Instead it would contain an
entire `PrimExpr`.  This is what HaskellDB did with it's `Expr` type.
