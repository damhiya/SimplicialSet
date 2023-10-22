{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        ghcWithPackages = pkgs.haskell.packages.ghc963.ghcWithPackages;
        ghc = ghcWithPackages (pkgs: with pkgs; [ ghc-typelits-natnormalise ]);
      in {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [ ];
          buildInputs = [ ghc ];
        };
      });

}
