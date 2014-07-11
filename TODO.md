# Adding support for SQL features currently missing

## Outer joins

There is a hacky version of `LEFT OUTER JOIN` in
`Karamaan.Opaleye.LeftJoin`.  It doesn't use the AST but instead uses
a trick to embed the SQL directly.  It is not the correct long-term
solution but will do for now.  When a non-hacky version is implemented
the API will not need to change.  `FULL OUTER JOIN` can be implemented
exactly the same way.

A non-hacky version will be a lot of work.  It requires hacking both
HaskellDB's `PrimQuery` and `SqlSelect` to support them.  The Opaleye
side doesn't pose much of a problem.

It can be done by someone dedicated enough who's willing to spend the
time to get their hands dirty with the internals.

## `LIMIT`

This should be pretty easy.  It requires a function of type

    limit :: Int -> Query a -> Query a

that modifies a `PrimQuery` by doing

    \n q -> Special (Top n) q

That actually may be all that's necessary!

## `OFFSET`

This is not supported by HaskellDB but probably will be simple to add.
A new constructor to `SpecialOp` needs to be added, say `Offset Int`.
Then we do exactly the same as for LIMIT.

There's one potential obstacle: `limit m . offset n` may end up
generating invalid SQL because they're the "wrong way round".  I don't
think you can say

    SELECT ... FROM ... OFFSET n LIMIT m

in SQL.  Only `LIMIT m OFFSET n` is valid, as far as I know.  This may
end up confusing `defaultSqlSpecial`.  A potential way around this
would be to replace the `Top Int` constructor with `LimitOffset (Maybe
Int) (Maybe Int)` to ensure that `defaultSqlSpecial` can always deal
with both at once.

Anyway, that's an interesting problem for someone interested to work
on.


## `ORDER BY`

This should be pretty easy.  It will use the `Order [OrderExpr]`
constructor of `SpecialOp`.  An `OrderExpr` is `OrderExpr OrderOp
PrimExpr`.  The `OrderOp` is either `OpAsc` or `OpDesc`.  We should
create the `PrimExpr` using an `ExprArr`.  I imagine the necessary
combinator would look like this

    orderBy :: Query a -> [OrderSpec a] -> Query a

`OrderSpec` would be an abstract datatype that should be something
like

    data OrderSpec a = OrderSpec OrderOp (a -> PrimQuery)

and we form an `OrderSpec` by through

    makeOrderSpec :: OrderOp -> ExprArr a (Wire b) -> OrderSpec a

for which we will use `runExprArr''` with an empty scope.

