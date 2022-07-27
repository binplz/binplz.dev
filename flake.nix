{
  description = "binplz";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: { };
      perSystem = system: { };
    in
    inputs.flake-utils.lib.eachDefaultSystem perSystem // overlay;

}
