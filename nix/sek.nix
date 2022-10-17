{ buildDunePackage, fetchgit, cppo, pprint, seq, monolith }:
buildDunePackage (rec {
  pname = "sek";
  version = "20201012";
  useDune3 = true;
  minimalOCamlVersion = "4.04";

  src = fetchgit {
    url = "https://gitlab.inria.fr/fpottier/sek.git";
    rev = "refs/tags/${version}";
    sha256 = "sha256-tReBlvpimy8kMEf4ai4hoZDnGgSta0jjFVYWNwEPGA4=";
  };

  nativeBuildInputs = [ cppo ];
  propagatedBuildInputs = [ pprint seq monolith ];
  strictDeps = true;
})
