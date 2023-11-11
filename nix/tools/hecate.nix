{ inputs, buildPythonPackage, tmux, pytest }:

buildPythonPackage rec {
  pname = "hecate";
  version = "latest";
  src = inputs.hecate;
  propagatedBuildInputs = [ tmux pytest ];
}
