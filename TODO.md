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
