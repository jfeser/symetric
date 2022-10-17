{ buildDunePackage, fetchFromGitHub, base, ppx_js_style, ppx_yojson_conv_lib }:
buildDunePackage (rec {
  pname = "ppx_yojson_conv";
  version = "0.15";
  useDune3 = true;
  minimalOCamlVersion = "4.04";

  src = fetchFromGitHub {
    owner = "janestreet";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-lSOUSMVgsRiArEhFTKpAj2yFBPbtaIc/SxdPA+24xXs=";
  };

  buildInputs = [ ppx_js_style ];
  propagatedBuildInputs = [ base ppx_yojson_conv_lib ];
  strictDeps = true;
})
