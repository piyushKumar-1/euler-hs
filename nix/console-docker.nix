{ pkgs ? import <nixpkgs> {}
, version
}:
with pkgs;
let
  nixFromDockerHub = dockerTools.pullImage {
    imageName = "nixos/nix";
    imageDigest = "sha256:5d0a26066dc8e9e1295676203cbc15d0e124f445007f9cb888fde25c3d00a1b2";
    sha256 = "04k94d1sxzsa467p451xs412qp50rs8id49d7dqgxghjr8ygk7js";
    finalImageTag = "latest";
  };
  euler-hs-pkgs =
    import ../default.nix { withHoogle = false; };
in
dockerTools.buildImage {
  name = "asia.gcr.io/jp-k8s-internal/console";
  created = "now";
  tag = version;
  fromImage = nixFromDockerHub;
  contents = [
    bashInteractive_5
    iana-etc
    cacert
    bash
    coreutils
    curl
  ];
  config = {
    Cmd = [ "${euler-hs-pkgs.console}/bin/console" ];
  };
}
