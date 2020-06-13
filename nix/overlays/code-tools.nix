{ eulerBuild }:
let
  cabal-fmt-repo = eulerBuild.fetchFromGitHub {
    owner = "phadej";
    repo = "cabal-fmt";
    rev = "59387f9515aa678fd59ddc426591a2b65c867e95";
    sha256 = "1dz9ldmqy46npyhy93xykyjdinz7gm1zhgp4kwl22w8bnpf793in";
  };
  cabal-fmt-path = cabal-fmt-repo;
in
eulerBuild.mkHaskellOverlay
  (self: super: hself: hsuper: {
    cabal-fmt = eulerBuild.fastBuildExternal {
      drv = hself.callCabal2nix "cabal-fmt" cabal-fmt-path { };
    };
  })
      
