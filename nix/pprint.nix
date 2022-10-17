{ buildDunePackage, fetchFromGitHub }:
buildDunePackage (rec {
  pname = "pprint";
  version = "20220103";
  useDune3 = true;
  minimalOCamlVersion = "4.04";

  src = fetchFromGitHub {
    owner = "fpottier";
    repo = pname;
    rev = "${version}";
    sha256 = "sha256-XW1iMpyskaKxedhF8s6ZVNqDUsoBG0AOqS42Ki23xic=";
  };

  buildInputs = [ ];
  propagatedBuildInputs = [ ];
  strictDeps = true;
})
