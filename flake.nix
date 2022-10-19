{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
        overlay = self: super: {
          ppx_yojson_conv = self.callPackage ./nix/ppx_yojson_conv.nix { };
          sek = self.callPackage ./nix/sek.nix { };
          pprint = self.callPackage ./nix/pprint.nix { };
        };
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages.overrideScope' overlay;
        symetric = ocamlPkgs.buildDunePackage rec {
          pname = "symetric";
          version = "0.1";
          useDune3 = true;
          minimalOCamlVersion = "4.13";
          nativeBuildInputs = [
            ocamlPkgs.core
            ocamlPkgs.core_bench
            ocamlPkgs.core_unix
            ocamlPkgs.ppx_yojson_conv
            ocamlPkgs.menhir
            ocamlPkgs.fmt
            ocamlPkgs.yojson
            ocamlPkgs.gen
            ocamlPkgs.iter
            ocamlPkgs.bheap
            ocamlPkgs.logs
            combinat.defaultPackage.${system}
            bitarray.defaultPackage.${system}
            vp-tree.defaultPackage.${system}
          ];
          propagatedBuildInputs =
            [ ocamlPkgs.base ocamlPkgs.fmt ocamlPkgs.sek ];
          src = ./.;
        };
      in {
        packages = { symetric = symetric; };
        defaultPackage = symetric;
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            [ pkgs.ocamlformat pkgs.opam pkgs.ocamlPackages.ocaml-lsp ];
          inputsFrom = [ symetric ];
        };
      });
}
