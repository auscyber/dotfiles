{ config, pkgs, ... }:
{
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    #    extraConfig = ''
    #      let g:sqlite_clib_path = "${pkgs.sqlite.out}/lib/libsqlite3.so"
    #    '';
  };
}
