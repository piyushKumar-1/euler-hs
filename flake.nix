# Autogenerated from euler.yaml. Do not edit.
# hash: ee55ac2e0a6027f8e09fdeeb2198cb79ff44740c4b534869f2564c7d0b114dfa
# time: 2020-10-27 14:33:56.585119

{
  description = "euler-hs";
  inputs = {
    # Laziness of nix allows us to be lazy hear and not to resolve deps
    # The downside is that most of this .follows are redundant
    euler-build.inputs.beam.follows = "beam";
    euler-build.inputs.beam-mysql.follows = "beam-mysql";
    euler-build.inputs.haskell-sequelize.follows = "haskell-sequelize";
    beam.inputs.euler-build.follows = "euler-build";
    beam.inputs.beam-mysql.follows = "beam-mysql";
    beam.inputs.haskell-sequelize.follows = "haskell-sequelize";
    beam-mysql.inputs.euler-build.follows = "euler-build";
    beam-mysql.inputs.beam.follows = "beam";
    beam-mysql.inputs.haskell-sequelize.follows = "haskell-sequelize";
    haskell-sequelize.inputs.euler-build.follows = "euler-build";
    haskell-sequelize.inputs.beam.follows = "beam";
    haskell-sequelize.inputs.beam-mysql.follows = "beam-mysql";
  };

  outputs = flakeInputs@{ self, euler-build, ... }:
    euler-build.mkEulerFlake {
      overlayPath = ./nix/overlay.nix;
      mkConfig = { nixpkgs }: {
        flakeName = "euler-hs";
        defaultPackageName = "euler-hs";
        exportPackages = [
          "euler-hs"
        ];
        shellTools = with nixpkgs; [
          
        ];
        # shellAttrs = {
        # };
      };
      inputs = flakeInputs;
    };
}
