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
  };
  outputs = { self, flake-utils, nixpkgs, bitarray, combinat, vp-tree }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
        symetric = ocamlPkgs.buildDunePackage rec {
          pname = "symetric";
          version = "0.1";
          duneVersion = "3";
          propagatedBuildInputs = [
            ocamlPkgs.core
            ocamlPkgs.core_bench
            ocamlPkgs.core_unix
            ocamlPkgs.ppx_yojson_conv
            ocamlPkgs.menhir
            ocamlPkgs.fmt
            ocamlPkgs.yojson
            ocamlPkgs.iter
            ocamlPkgs.bheap
            ocamlPkgs.logs
            # combinat.defaultPackage.${system}
            # bitarray.defaultPackage.${system}
            # vp-tree.defaultPackage.${system}
          ];
          src = ./.;
        };
      in {
        packages = { symetric = symetric; };
        defaultPackage = symetric;
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
          inputsFrom = [ symetric ];
        };
      });
}
