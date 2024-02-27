{
  perSystem = { simpleHaskellNix, self', pkgs, config, ... }:
    let
      cardanoPackages = pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-haskell-packages";
        rev = "3df392af2a61d61bdac1afd9c3674f27d6aa8efc"; # branch: repo
        hash = "sha256-vvm56KzA6jEkG3mvwh1LEdK4H4FKxeoOJNz90H8l8dQ=";
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
      devShells.plutusTest = plutusTest.devShell;
      inherit (plutusTest) packages;
    };
}
