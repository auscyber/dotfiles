{ config, pkgs, ... }:

{
  sops.templates."extraNix.conf".content = ''
    	access-tokens = github.com=${config.sops.placeholder.github_token}
  '';
  nix.extraOptions = ''
    !include ${config.sops.templates."extraNix.conf".path}
        		'';
}
