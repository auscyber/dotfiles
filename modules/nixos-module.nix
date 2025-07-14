{
  flake.modules.nixos.no-git-metadata = {
    system = {
      nixos = {
        label = "no_git_metadata";
        version = "no_git_metadata";
      };
      tools.nixos-version.enable = false;
    };
  };
}
