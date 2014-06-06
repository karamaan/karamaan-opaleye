{}:
with import <nixpkgs> {};

let 
    haskellPackages = pkgs.haskellPackages_ghc763.override {
      extension = self: super: {
        haskelldb = self.callPackage <haskelldb> {};
        productProfunctors = self.callPackage <product-profunctors> {};
        karamaanPlankton = self.callPackage <karamaan-plankton> {};
        karamaanOpaleye = self.callPackage ./. {};
        
      };
    };
in lib.overrideDerivation haskellPackages.karamaanOpaleye (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_20_0_1 ] ++ attrs.buildInputs;
   })
