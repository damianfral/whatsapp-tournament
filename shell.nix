# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "whatsapp-tournament";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ parsec tabular time ];
  meta = {
    homepage = "http://github.com/&quot;damianfral&quot;/whatsapp-tournament";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})