{ inputs, ... }:
{
  input-branches = {
    inputs = {
      attic = {
        upstream = {
          url = "https://github.com/zhaofengli/attic.git";
          ref = "main";
        };
      };

      input-branches = {
        upstream = {
          url = "https://github.com/mightyiam/input-branches.git";
          ref = "main";
        };
      };
      darwin = {
        upstream = {
          url = "https://github.com/nix-darwin/nix-darwin.git";
          ref = "master";
        };

        shallow = true;
      };
      agenix = {
        upstream = {
          url = "https://github.com/ryantm/agenix.git";
          ref = "main";
        };
      };
      agenix-rekey = {
        upstream = {
          url = "https://github.com/oddlama/agenix-rekey.git";
          ref = "main";
        };
      };
      home-manager = {
        upstream = {
          url = "https://github.com/nix-community/home-manager.git";
          ref = "master";
        };
      };
      age-plugin-gpg = {
        upstream = {
          url = "https://github.com/certainlach/age-plugin-gpg.git";
          ref = "main";
        };
      };

      #	shallow = true;

    };
    repoPath = "https://github.com/auscyber/dotfiles.git";
  };
}
