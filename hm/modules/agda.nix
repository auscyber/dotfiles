{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    (agda.withPackages (
      p: with p; [
        standard-library
        cubical
        agda-categories
      ]
    ))
  ];
}
