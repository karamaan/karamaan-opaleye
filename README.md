You will need the following libraries

* [https://github.com/karamaan/karamaan-plankton](https://github.com/karamaan/karamaan-plankton)
* [https://github.com/karamaan/product-profunctors](https://github.com/karamaan/product-profunctors)

You should also use the Github version of HaskellDB available here

* [https://github.com/tomjaguarpaw/haskelldb](https://github.com/tomjaguarpaw/haskelldb)

which will be kept up-to-date with any patches needed to support new
functionality in Opaleye.  (You should update karamaan-opaleye.cabal
to depend on the version of haskelldb from that repository.)

Some brief release notes:

* The software is not polished at all, but we are using in in production.

* There's little documentation, but [I am happy to answer
  questions](http://web.jaguarpaw.co.uk/~tom/contact/) by email on a
  very prompt basis.  Don't be shy and don't
  be worried about taking up my time.  Please fire off an email to me
  at any time with questions or comments.

* The best place to start is in Examples.lhs.

* I recommend you get comfortable with writing Opaleye queries first by
  compiling them to SQL with 'showSqlForPostgres'.  Later you can use
  RunQuery.hs to actually run them against Postgres.

* Data manipulation (DELETE, INSERT, UPDATE) is supported but not documented
  at all.

* You'll probably benefit from ensuring you are looking at [the `dev`
  branch](https://github.com/karamaan/karamaan-opaleye/tree/dev) as it
  has the latest features.  `master` is usually a few weeks behind.
  If you want to submit any pull requests please make them against the
  dev branch.

* Please join the [Opaleye mailing list](https://lists.sourceforge.net/lists/listinfo/opaleye-users)
