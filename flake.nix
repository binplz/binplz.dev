{
  description = "binplz-server";

  inputs.nixpkgs = { };
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              binplz-server = hfinal.callCabal2nix "binplz-server" ./binplz-server { };
            };
        };
        binplz-server = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.binplz-server;
        binplz-docs = final.callPackage ../docs { };
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.binplz-server ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.bashInteractive
              pkgs.rlwrap
              pkgs.sqlite
            ];
          };
          defaultPackage = pkgs.binplz-server;
          packages = {
            docs = pkgs.binplz-docs;
            server = pkgs.binplz-server;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
