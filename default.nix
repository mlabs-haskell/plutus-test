{ self, ... }:
{
  perSystem = { simpleHaskellNix, self', pkgs, config, ... }:
    let
      cardanoPackages = pkgs.fetchFromGitHub {
        owner = "IntersectMBO";
        repo = "cardano-haskell-packages";
        rev = "3167b742cea332e1c978d8ecc69ef8d6bd0d6e19"; # branch: repo
        hash = "sha256-oCObuK/TY71lL+vDiRT0/Hhrsq4GRC7n8kcKBeonoUk=";
      };

      plutusTest = simpleHaskellNix.mkPackage {
        name = "plutus-test";
        src = ./.;

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
        };
      };
    in
    {
      devShells.plutusTest = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        inputsFrom = [
          plutusTest.devShell
        ];
      };
      inherit (plutusTest) packages checks;
    };
}
