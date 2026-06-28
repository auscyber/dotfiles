{ den, ... }:
{
  den.aspects.agda = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          (agda.withPackages (p: with p; [
            standard-library
            cubical
            agda-categories
          ]))
        ];
      };
  };
}
