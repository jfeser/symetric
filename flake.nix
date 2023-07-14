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
    sketch.url = "github:jfeser/sketch-nix";
    sketch.inputs.nixpkgs.follows = "nixpkgs";
    sketch.inputs.flake-utils.follows = "flake-utils";
  };
  outputs =
    { self, flake-utils, nixpkgs, bitarray, combinat, vp-tree, sketch }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev: {
            ocaml = oprev.ocaml.override { flambdaSupport = true; };
            symetric-lib = ofinal.buildDunePackage rec {
              pname = "symetric-lib";
              version = "0.1";
              duneVersion = "3";
              nativeBuildInputs = [ ofinal.menhir ];
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
                ofinal.bitarray
                ofinal.combinat
                ofinal.vpt
              ];
              src = ./.;
            };

            symetric = ofinal.buildDunePackage rec {
              pname = "symetric";
              version = "0.1";
              duneVersion = "3";
              buildInputs = [ ofinal.core_unix ofinal.symetric-lib ];
              src = ./.;
              doNixSupport = false;
            };
          });
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            bitarray.overlays.${system}.default
            combinat.overlays.${system}.default
            vp-tree.overlays.${system}.default
            overlay
          ];
        };
        parallel = pkgs.parallel.override {
          procps = pkgs.procps.override { withSystemd = false; };
        };
      in {
        packages = {
          symetric = pkgs.ocamlPackages.symetric;
          symetric-lib = pkgs.ocamlPackages.symetric-lib;

          buildContainer = pkgs.dockerTools.streamLayeredImage {
            name = "jfeser/symetric";
            tag = "latest";
            contents = [
              self.packages.${system}.symetric
              sketch.defaultPackage.${system}
              pkgs.procps
              pkgs.busybox
              pkgs.xonsh
              pkgs.gnumake
              pkgs.perl
              parallel
            ];
            config = {
              Cmd = [ "${pkgs.bashInteractive}/bin/bash" ];
              WorkingDir = "/work";
            };
          };
        };
        overlays.default = overlay;
        defaultPackage = self.packages.${system}.symetric-lib;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.odoc
            pkgs.openai
            pkgs.python3Packages.jupyter
            pkgs.python3Packages.matplotlib
            pkgs.python3Packages.docopt
            pkgs.python3Packages.pandas
            pkgs.python3Packages.tqdm
            pkgs.parallel
            pkgs.xonsh
          ];
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
