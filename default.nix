with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "purescript";
  env = buildEnv { name = name; paths = buildInputs; };

  purescript = (haskellPackages.override {
    overrides = self: super: {
      purescript = haskell.lib.overrideCabal super.purescript (p: {
        version = "0.12.0";
        sha256 = "0lkrlry4rr1l1c5ncy7wlbv1ll6n0dkw7j1gjpxn3706gan921rb";
      });
    };
  }).purescript;  
  
  buildInputs = with pkgs; [
    purescript
    psc-package
    chromium
    nodejs-8_x
    inkscape
    imagemagick
    gimp
  ];

  shellHook = "
    export PATH=$PWD/node_modules/.bin:$PATH
    export CHROME_BIN=$(which chromium)
  ";
}
