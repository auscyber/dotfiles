{ config, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    vimAlias = true;
    #    package = pkgs.neovim-nightly;
    #    extraConfig = ''
    #      let g:sqlite_clib_path = "${pkgs.sqlite.out}/lib/libsqlite3.so"
    #    '';
  };
}
