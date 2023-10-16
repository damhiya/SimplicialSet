{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        hspkgs = pkgs: with pkgs; [ ];
      in {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [ ];
          buildInputs = [ (pkgs.ghc.withPackages hspkgs) ];
        };
      });

}
