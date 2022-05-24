let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  haskellDeps = ps: with ps; [ base hspec http-conduit rio ];

  ghc = pkgs.haskell.compiler.ghc922 haskellDeps;

  inputs = [ pkgs.gcc pkgs.ghc pkgs.ghcid pkgs.stack pkgs.llvm pkgs.nixfmt ];

  hooks = ''
    mkdir -p .nix-stack
    export STACK_ROOT=$PWD/.nix-stack
  '';
in pkgs.stdenv.mkDerivation {
  name = "arbiter";
  src = ./.;
  buildInputs = inputs;
  shellHook = hooks;
}
