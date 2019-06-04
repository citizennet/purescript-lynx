let

  buildInputs = [
    nixpkgs.coreutils
    nixpkgs.findutils
    nixpkgs.git
    nixpkgs.gnumake
    nixpkgs.nodejs-10_x
    nixpkgs.yarn
  ];

  nixpkgs = import nixpkgs-tarball {};

  nixpkgs-tarball = builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${revision}.tar.gz";
  };

  revision = "7f35ed9df40f12a79a242e6ea79b8a472cf74d42";

  sha256 = "sha256:1wr6dzy99rfx8s399zjjjcffppsbarxl2960wgb0xjzr7v65pikz";

in

  nixpkgs.stdenv.mkDerivation rec {
    inherit buildInputs;

    env = nixpkgs.buildEnv {
      inherit name;

      paths = buildInputs;
    };

    name = "purescript-lynx";
  }
