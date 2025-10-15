{
  self,
  inputs,
  lib,
  ...
}:

{

  flake = {

  };
  perSystem =
    { pkgs, system, ... }:
    {

      checks.ci =
        with builtins;
        let
          isDerivation = p: isAttrs p && p ? type && p.type == "derivation";
          isBuildable =
            p:
            let
              licenseFromMeta = p.meta.license or [ ];
              licenseList = if builtins.isList licenseFromMeta then licenseFromMeta else [ licenseFromMeta ];
            in
            !(p.meta.broken or false)
            && builtins.all (license: license.free or true) licenseList
            && builtins.any (platform: platform == system) p.meta.platforms;
          isCacheable = p: !(p.preferLocalBuild or false);
          # get all systems and exposed packages and shells from this repo
          systemConfigurations = lib.filterAttrs (_: v: v.system.system == system) (
            if pkgs.stdenv.isDarwin then self.darwinConfigurations else self.nixosConfigurations
          );
          systemBuilds = lib.mapAttrsToList (_: v: v.system) systemConfigurations;
          nixShells = lib.attrValues self.devShells."${pkgs.stdenv.system}";
          homes = lib.attrValues (
            lib.filterAttrs (_: v: v.system == system) (
              builtins.mapAttrs (_: v: v.activationPackage) self.homeConfigurations
            )
          );
          packages = builtins.filter (v: isBuildable v && isCacheable v && isDerivation v) (
            lib.attrValues (lib.filterAttrs (name: _: name != "ci") self.packages."${pkgs.stdenv.system}")
          );
          outputsOf = p: map (o: p.${o}) p.outputs;
          outputPkgs = lib.concatLists [
            homes
            systemBuilds
            packages
            nixShells
          ];

        in
        concatMap outputsOf outputPkgs;

    };

}
