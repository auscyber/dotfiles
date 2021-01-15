{pkgs, lib, ... }:

{
  enable = true;
  plugins = [
    
      {
	name = "zsh-syntax-highlighting";
	src = pkgs.fetchFromGitHub {
	  owner = "zsh-users";
	  repo = "zsh-syntax-highlighting";
	  rev = "5eb494852ebb99cf5c2c2bffee6b74e6f1bf38d0";
	  sha256 = "03vszkmm09smbzpbnk6c5f5sx1y9vh0j91xvgqfv6m4gldxrj37j";
	};
      }
   

  ];
  enableAutosuggestions = true;
  enableCompletion = true;
  sessionVariables = {
    PATH = "$PATH:/home/auscyber/.emacs.d/bin";
    EDITOR = "nvim";
  };
  shellAliases = {
    e = "vim";
    t = "tmux";
    hm = "home-manager";
    ghc_env = ''{
     nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; $1)" 
    }
    '';
    emacs = "emacsclient -t";
  };
  initExtra = ''
setopt SOURCE_TRACE
echo hello
. ${pkgs.fzf}/share/fzf/completion.zsh
. ${pkgs.fzf}/share/fzf/key-bindings.zsh
  '';

}
