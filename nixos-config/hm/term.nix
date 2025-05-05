{ config
, nixpkgs
, ...
}: {

  programs.ghostty = {

    enable = true;
    themes = {
      pink_ocean = {

        foreground = "d0d0d0";
        cursor-color = "eeeeee";
        selection-background = "005f5f";
        selection-foreground = "eeeeee";
        palette = [
          "0=#080808"
          "1=#ff5f5f"
          "2=#87d7af"
          "3=#d7d787"
          "4=#5fafd7"
          "5=#afafff"
          "6=#5fd7d7"
          "7=#dadada"
          "8=#8a8a8a"
          "9=#d75f5f"
          "10=#afd7af"
          "11=#d7d7af"
          "12=#87afd7"
          "13=#afafd7"
          "14=#87d7d7"
          "14=#dadada"
        ];
        background = "202020";

      };




    };
    settings = {
      theme = "pink_ocean";
      background-opacity = 0.8;
      font-size = 15;
      font-family = "Hasklug Nerd Font";
    };
  };
}
