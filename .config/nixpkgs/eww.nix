{pkgs, ...}:
with pkgs;
rustPlatform.buildRustPackage rec {
  pname = "eww";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = "0d2ee78f91070c93855ea14e04c4b8b70161877f";
    sha256 = "1hqps7l5qrjh9f914r5i6kmcz6f1yb951nv4lby0cjnp5l253kps";
  };

  cargoSha256 = "3IJ/zYhZGCrJnxPqaGQaB5kT6n9cO1zF7IDJV6foSac=";


}
