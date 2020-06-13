{
  eulerBuild
, src
}:
let
  hedis-repo = eulerBuild.fetchFromGitHub {
    owner = "juspay";
    repo = "hedis";
    rev = "4ea54f16c0057acc99a9f0e9b63ea51ea4bf420e";
    sha256 = "094r4pxkc3h6w2vy4lha1zfdz29qihvkx2wi3mb7g1m3a6c7xp4h";
  };

  hedis-path = hedis-repo;
in
eulerBuild.mkEulerHaskellOverlay
  (self: super: hself: hsuper: {
    hedis =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "hedis" hedis-path { };
      };

    euler-hs =
      eulerBuild.fastBuild {
        drv = hself.callCabal2nix "euler-hs" src { };
      };

    # TODO: remove dontCheck above and remove this package
    # after fixing call untyped API test
    euler-hs-with-tests =
      eulerBuild.fastBuild {
        drv = hself.callCabal2nix "euler-hs" src { };
        overrides = {
          runTests = true;
        };
      };
  })
