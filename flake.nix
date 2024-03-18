{
  description = "pure-sum";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskellPackages;
      in
      rec
      {
        packages.pure-sum =
          haskellPackages.callCabal2nix "pure-sum" ./pure-sum {
          };
        packages.pure-sum-aeson =
          haskellPackages.callCabal2nix "pure-sum-aeson" ./pure-sum-aeson {
            pure-sum = packages.pure-sum;
          };

        defaultPackage = pkgs.linkFarmFromDrvs "all-pure-sum" (pkgs.lib.unique (builtins.attrValues packages));


        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            haskell-ci
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
