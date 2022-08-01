{
  description = "binplz";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = import nix/overlay.nix;
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
        in
        {
          # defaultPackage = pkgs.purenix;
          # packages.purenix = pkgs.purenix;

          packages.docs = pkgs.binplz-docs;

          # devShell = pkgs.hacking-on-purenix-shell;
          # devShells = { };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
