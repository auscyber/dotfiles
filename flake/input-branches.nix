{ inputs, ... }:
{
  input-branches.inputs = {
    darwin = {
      upstream = {
        url = "https://github.com/nix-darwin/nix-darwin.git";
        ref = "master";
      };

      shallow = true;
    };

  };
}
