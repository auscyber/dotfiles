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

      packages.ci = pkgs.nix-fast-build;
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
          systemConfigurations = lib.filterAttrs (_: v: v.config.system.build.toplevel.system == system) (
            if pkgs.stdenv.isDarwin then self.darwinConfigurations else self.nixosConfigurations
          );
          systemBuilds = lib.mapAttrsToList (_: v: v.config.environment.systemPackages) systemConfigurations;
          nixShells = lib.attrValues self.devShells."${pkgs.stdenv.system}";
          homes = lib.attrValues (
            lib.filterAttrs (_: v: v.system == system) (
              builtins.mapAttrs (_: v: v.config.home.packages) self.homeConfigurations
            )
          );
          packages = builtins.filter (v: isBuildable v && isCacheable v && isDerivation v) (
            lib.attrValues (lib.filterAttrs (name: _: name != "ci") self.packages."${pkgs.stdenv.system}")
          );
          systemPackages = lib.concatLists [
            homes
            systemBuilds
          ];
          outputsOf = p: map (o: p.${o}) p.outputs;
          outputPkgs = lib.concatLists [
            (lib.concatLists systemPackages)
            packages
            nixShells
          ];

        in
        #        builtins.listToAttrs (
        #          builtins.map (drv: {
        #            name = drv.name;
        #            value = drv;
        #}) (
        pkgs.stdenv.mkDerivation {
          name = "ci-checks-${system}";
          buildInputs = outputPkgs;
        }
      #		  ))
      ;

    };

}
