{
  nixosModuleAttr,
  noGitMetadataPlaceholder,
  lib,
  ...
}:
{
  _module.args = {
    nixosModuleAttr = "default";
    noGitMetadataPlaceholder = "no_git_metadata";
  };

  flake.modules.nixos.${nixosModuleAttr} = {
    nixpkgs.flake.source = lib.mkOverride 90 null;
    system = {
      nixos = {
        label = noGitMetadataPlaceholder;
        version = noGitMetadataPlaceholder;
      };
      tools.nixos-version.enable = false;
    };
  };
}
