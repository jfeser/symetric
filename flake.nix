{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    combinat.url = "github:jfeser/combinat";
    combinat.inputs.nixpkgs.follows = "nixpkgs";
    bitarray.url = "github:jfeser/bitarray";
    bitarray.inputs.nixpkgs.follows = "nixpkgs";
    vp-tree.url = "github:jfeser/vp-tree";
    vp-tree.inputs.nixpkgs.follows = "nixpkgs";
    ancient.url = "github:jfeser/ocaml-ancient";
    ancient.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { self, flake-utils, nixpkgs, bitarray, combinat, vp-tree, ancient }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev: {
            ocaml = oprev.ocaml.override { flambdaSupport = true; };
            symetric = ofinal.buildDunePackage rec {
              pname = "symetric";
              version = "0.1";
              duneVersion = "3";
              propagatedBuildInputs = [
                ofinal.core
                ofinal.core_bench
                ofinal.core_unix
                ofinal.ppx_yojson_conv
                ofinal.menhir
                ofinal.fmt
                ofinal.yojson
                ofinal.iter
                ofinal.bheap
                ofinal.logs
                ofinal.ancient
                ofinal.bitarray
                ofinal.combinat
                ofinal.vpt
              ];
              src = ./.;
            };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            ancient.overlays.${system}.default
            bitarray.overlays.${system}.default
            combinat.overlays.${system}.default
            vp-tree.overlays.${system}.default
            overlay
          ];
        };
      in {
        packages = { symetric = pkgs.ocamlPackages.symetric; };
        overlays.default = overlay;
        defaultPackage = self.packages.${system}.symetric;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.odoc
            pkgs.openai
            pkgs.python3Packages.docopt
            pkgs.python3Packages.pandas
            pkgs.python3Packages.tqdm
          ];
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
