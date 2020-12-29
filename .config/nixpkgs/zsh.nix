{pkgs, lib, ... }:

{
  enable = true;
  plugins = [
    { 
	name = "zsh-autosuggestions";
	src = pkgs.fetchFromGitHub {
	  owner = "zsh-users";
	  repo = "zsh-autosuggestions";
	  rev = "ae315ded4dba10685dbbafbfa2ff3c1aefeb490d";
	  sha256 = "0h52p2waggzfshvy1wvhj4hf06fmzd44bv6j18k3l9rcx6aixzn6";
	};
      }
      {
	name = "zsh-syntax-highlighting";
	src = pkgs.fetchFromGitHub {
	  owner = "zsh-users";
	  repo = "zsh-syntax-highlighting";
	  rev = "5eb494852ebb99cf5c2c2bffee6b74e6f1bf38d0";
	  sha256 = "03vszkmm09smbzpbnk6c5f5sx1y9vh0j91xvgqfv6m4gldxrj37j";
	};
      }
      { name ="zsh-completions";
	src =  pkgs.fetchFromGitHub {
	  owner = "zsh-users";
	  repo = "zsh-completions";
	  rev = "0.31.0";
	  sha256 = "0rw23m8cqxhcb4yjhbzb9lir60zn1xjy7hn3zv1fzz700f0i6fyk"; 
	};
      }

  ];
  shellAliases = {
    e = "vim";
    t = "tmux";
    ghc_env = ''{
     nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; $1)" 
    }
	
    '';
    emacs = "emacsclient -c -s /tmp/emacs1000/server";
  };
  initExtra = ''
    eval "$(starship init zsh)"
  '';

}
