{
  eulerBuild
, src
}:
let
  hedis-repo = eulerBuild.fetchFromGitHub {
    owner = "juspay";
    repo = "hedis";
    rev = "46ea0ea78e6d8d1a2b1a66e6f08078a37864ad80";
    sha256 = "1xql164afmjc44s5p8ngws8y6jmb071nqdyrh0v5kz0jbb315595";
  };

  hedis-path = hedis-repo;

in
eulerBuild.mkEulerHaskellOverlay
  (self: super: hself: hsuper: {
    # this is needed for euler-db and euler-types currently
    # TODO: remove after updating nixpkgs
    record-dot-preprocessor =
      eulerBuild.fastBuildExternal {
        drv = hself.callHackageDirect {
          pkg = "record-dot-preprocessor";
          ver = "0.2.6";
          sha256 = "1mp2bcb3rkg33z2030267pjvbbzadbh8cmrsax25ygk9bxmx8bbf";
        } { };
      };

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
