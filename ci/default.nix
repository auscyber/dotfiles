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
      checks =
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
          systemConfigurations = lib.filterAttrs (_: v: v.config.nixpkgs.system == system) (
            if pkgs.stdenv.isDarwin then self.darwinConfigurations else self.nixosConfigurations
          );
		  systemBuilds = lib.concatMapAttrs (name: v: { "check-system-builds-${name}" = v.config.system.build.toplevel; } ) systemConfigurations;
#          systemPackages = lib.concatMapAttrs (name: v: builtins.listToAttrs (builtins.map (v: { "check-package-${v.name}" = v;})) v.config.environment.systemPackages ) systemConfigurations;
          nixShells = lib.concatMapAttrs (name: v: { "check-shell-${name}" = v; }) self.devShells."${system}";
          homes = lib.filterAttrs (_: v: v.activationPackage.system == system) self.homeConfigurations;
#		  homePackages = lib.concatMapAttrs (name: v: builtins.listToAttrs (builtins.map (v: {"check-package-${v.name}" = v;}) v.config.home.packages) ) homes;
		  homeBuilds = lib.concatMapAttrs (name: v: { "check-home-builds-${name}" = v.config.home.activationPackage; } ) homes;

          packages = builtins.filter (v: isBuildable v && isCacheable v && isDerivation v) (
            lib.attrValues (lib.filterAttrs (name: _: name != "ci") self.packages."${pkgs.stdenv.system}")
          );
		  packagesToBuild = builtins.listToAttrs (builtins.map ( v: { "check-package-${v.name}" = v; }) packages);

		  allBuilds = lib.mergeAttrsList [
			systemBuilds
			homeBuilds
		  ];

		  allPackages = lib.mergeAttrsList [
#			systemPackages
#			homePackages
			packagesToBuild
		  ];

		  drvNames = builtins.concatMap (p: [ p.name ]) (lib.attrValues allPackages) ++ builtins.concatMap (p: [ p.name ]) (lib.attrValues allBuilds);

		  uniqueDrvNames = builtins.removeDuplicates drvNames;

		  drvByName = builtins.listToAttrs (
			builtins.map (name: {
			  name = name;
			  value = builtins.findFirst (p: p.name == name) (lib.attrValues allPackages ++ lib.attrValues allBuilds);
			}) uniqueDrvNames
		  );
		  in
		  lib.mergeAttrsList [ allPackages allBuilds ];







        #        builtins.listToAttrs (
        #          builtins.map (drv: {
        #            name = drv.name;
        #            value = drv;
        #}) (

      #		  ))


    };

}
