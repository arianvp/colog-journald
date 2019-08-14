let pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "shell-file";
  buildInputs = [
    pkgs.zlib
    pkgs.gmp
    pkgs.ncurses
    pkgs.systemd.lib
    pkgs.systemd.dev
  ];
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc864
    pkgs.cabal-install
    pkgs.pkg-config
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.gmp}/lib:${pkgs.zlib}/lib:${pkgs.ncurses}/lib:${pkgs.systemd.lib}/lib
  '';
}

