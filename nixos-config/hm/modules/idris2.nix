{ config, pkgs, ... }:
{
  home.packages = with pkgs.idris2Pkgs; [ lsp Prettier ] ++ [pkgs.idris2];
}
