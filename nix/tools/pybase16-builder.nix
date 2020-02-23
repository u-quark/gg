{
  stdenv, buildPythonPackage, fetchPypi,
  aiofiles, pystache, pyyaml
}:

buildPythonPackage rec {
  pname = "pybase16-builder";
  version = "0.2.6";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0i12wnhm9nys4cjcvf558brbyc2xnf3mnl3shnnvrck2zidwmci0";
  };

  propagatedBuildInputs = [
    aiofiles
    pystache
    pyyaml
  ];
}
