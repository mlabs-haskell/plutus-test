{ self, inputs, ... }:
{
  perSystem = { simpleHaskellNix, self', pkgs, config, ... }:
    let
      plutusTest = simpleHaskellNix.mkPackage {
        name = "plutus-test";
        src = ./.;

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.cardanoPackages;
        };
      };
    in
    {
      devShells.plutusTest = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        inputsFrom = [
          plutusTest.devShells.default
        ];
      };
      inherit (plutusTest) packages checks;
    };
}
