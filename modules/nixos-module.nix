{ nixosModuleAttr, noGitMetadataPlaceholder, ... }:
{
  _module.args = {
    nixosModuleAttr = "no-git-metadata";
    noGitMetadataPlaceholder = "no_git_metadata";
  };

  flake.modules.nixos.${nixosModuleAttr} = {
    system = {
      nixos = {
        label = noGitMetadataPlaceholder;
        version = noGitMetadataPlaceholder;
      };
      tools.nixos-version.enable = false;
    };
  };
}
