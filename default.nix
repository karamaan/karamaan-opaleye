{ cabal, contravariant, haskelldb, karamaanPlankton, mtl
, postgresqlSimple, productProfunctors, profunctors, reflection
, tagged, text, time
}:

cabal.mkDerivation (self: {
  pname = "karamaan-opaleye";
  version = "0.21.2";
  src = ./.;
  buildDepends = [
    contravariant 
    haskelldb
    karamaanPlankton mtl postgresqlSimple
    productProfunctors profunctors reflection tagged text time
  ];
  doCheck = false;
  meta = {
    description = "A relational query language for Haskell";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
