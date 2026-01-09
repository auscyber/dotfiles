{ inputs, ... }:
{
  input-branches = {
    inputs = {
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

      #	shallow = true;

    };
    repoPath = "https://github.com/auscyber/dotfiles.git";
  };
}
