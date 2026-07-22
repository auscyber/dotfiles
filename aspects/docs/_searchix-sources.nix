{
  lib,
  pkgs,
  inputs,
  # Hosts whose option trees must NOT be evaluated here. The docs are derived
  # from `self.<class>Configurations.<host>.options`, so indexing the host that
  # *runs* searchix would make that host's module evaluation depend on its own
  # option tree (infinite recursion).
  excludeHosts ? [ "secondpc" ],
}:
# Builds the option sources that our searchix instance indexes. Each source is a
# directory holding `options.json` + `revision`, which is the layout searchix's
# `download` fetcher expects (it HTTP-GETs those two names under the source URL,
# see https://git.alin.ovh/searchix/blob/v0.4.4/defaults.toml).
#
# The point of doing it this way — rather than pointing searchix at the upstream
# NixOS/home-manager channels — is that these are *our* evaluated option trees:
# they contain the options our own aspects declare, plus every module we import
# (agenix, disko, impermanence, stylix, nixvim, ...), with the exact values and
# declaration sites this repo produces.
let
  self = inputs.self;
  selfPath = toString self;

  # searchix re-imports a source only when the revision it fetched last differs,
  # and declaration links are resolved against it, so the flake's git revision is
  # exactly the right value. A dirty tree still gets a distinct (unlinkable) id.
  revision = self.rev or self.dirtyRev or "dirty";

  repo = {
    type = "github";
    owner = "auscyber";
    repo = "dotfiles";
  };

  # Every host this flake defines, minus the ones we must not touch. Derived from
  # the flake rather than listed by hand so a new machine is indexed the moment
  # it exists — the point of this source set is to cover everything our aspects
  # *can* derive, and a hardcoded list silently stops being that.
  hostsOf = configs: lib.subtractLists excludeHosts (lib.attrNames configs);
  nixosHosts = hostsOf self.nixosConfigurations;
  darwinHosts = hostsOf self.darwinConfigurations;

  # A host contributes options through whichever aspects it happens to include,
  # so the union over all of them is strictly larger than any single host's tree:
  # nginx/celler/disko options only exist on the servers, sketchybar/rift/kanata
  # only on darwin, and so on.
  nixosConfigs = lib.genAttrs nixosHosts (h: self.nixosConfigurations.${h});
  darwinConfigs = lib.genAttrs darwinHosts (h: self.darwinConfigurations.${h});
  allConfigs = nixosConfigs // darwinConfigs;

  # home-manager (and therefore nixvim, which is imported from inside it) is
  # evaluated in the `home-manager.users` submodule, so its option tree is not
  # reachable from the host's `options`. `type.getSubOptions` is not enough
  # either: it only knows the submodule's *statically declared* modules, and our
  # HM content — including `imports = [ inputs.nixvim.homeModules.nixvim ]` — is
  # attached as a *definition*. (Checked: getSubOptions yields base HM but no
  # `programs.nixvim`.)
  #
  # So re-run the submodule evaluation ourselves, with the same payload (base HM
  # modules, specialArgs, class) and the same definitions the host merges in.
  # That reproduces the option tree the user actually gets on that machine.
  homeOptionsFor =
    cfg: user:
    let
      hmOption = cfg.options.home-manager.users;
      payload = hmOption.type.nestedTypes.elemType.functor.payload;
      definitions = builtins.filter (m: m != null) (map (def: def.${user} or null) hmOption.definitions);
    in
    (lib.evalModules {
      inherit (payload) class;
      specialArgs = payload.specialArgs // {
        name = user;
      };
      modules = payload.modules ++ definitions ++ [ { _module.args.name = user; } ];
    }).options;

  # Every (host, user) pair that actually has a home-manager configuration. The
  # same user on two hosts is two evaluations: they include different aspects.
  homeTargets = lib.concatLists (
    lib.mapAttrsToList (
      host: cfg:
      map (user: {
        inherit host user;
        options = homeOptionsFor cfg user;
      }) (lib.attrNames (cfg.config.home-manager.users or { }))
    ) allConfigs
  );

  # Declaration paths are absolute store paths after evaluation. Make ours
  # repo-relative so searchix's github link generation resolves them; strip the
  # `/nix/store/<hash>-` prefix from everything else so at least the in-tree path
  # of the declaring module stays readable.
  relativise =
    decl:
    let
      path = toString decl;
      inStore = builtins.match "/nix/store/[^/]+/(.*)" path;
    in
    if lib.hasPrefix "${selfPath}/" path then
      lib.removePrefix "${selfPath}/" path
    else if inStore != null then
      lib.head inStore
    else
      path;

  # Options whose default is a literal `abort` — rendering the docs forces it, and
  # unlike `throw` it cannot be caught, so these have to go by name.
  # agenix-rekey's storageMode nags this way until it is set explicitly.
  abortingOptions = [ "age.rekey.storageMode" ];

  # Options that throw when forced (agenix-rekey's secrets derivation: "Accessing
  # the secrets derivation is only possible when `storageMode` is set to
  # `derivation`"). One of them would otherwise take down the whole source.
  renderable = lib.filterAttrs (_: opt: (builtins.tryEval (builtins.deepSeq opt true)).success);

  # The flat `name -> option` attrset that upstream's options.json is made of.
  # Declarations are left as raw store paths here: whether an option is *ours* has
  # to be decided on those (several inputs — nixpkgs, disko — declare options from
  # their own `lib/`, so testing repo-relative paths would claim them as ours).
  optionsOf =
    options:
    renderable (removeAttrs (pkgs.nixosOptionsDoc { inherit options; }).optionsNix abortingOptions);

  # Options declared by this repo, as opposed to ones we inherit from an input.
  # Two shapes count as ours:
  #   * a file in the flake source (extraModules/, aspects/, ...); and
  #   * a den-synthesised module name like `os@vpn` or `nixos@overlays` — options
  #     declared inline in an aspect's class content have no file to point at, so
  #     den names them `<class>@<aspect>`. These are the most custom options we
  #     have, so missing them would rather defeat the purpose.
  isOurs =
    opt:
    lib.any (
      decl:
      let
        path = toString decl;
      in
      lib.hasPrefix "${selfPath}/" path || (!lib.hasPrefix "/" path && lib.hasInfix "@" path)
    ) (opt.declarations or [ ]);

  # Applied once the ownership question is settled.
  withLinkableDeclarations = lib.mapAttrs (
    _: opt: opt // { declarations = map relativise (opt.declarations or [ ]); }
  );

  nixvimPrefix = "programs.nixvim";

  # Union of the per-host flat option sets. Merging the rendered `name -> option`
  # attrsets (rather than the raw option trees) keeps this sound: each host's
  # tree comes out of its own module-system fixpoint, so they are only safely
  # comparable once `nixosOptionsDoc` has flattened them to plain data. Where two
  # hosts declare the same option, either rendering is equally true — they differ
  # at most in `default`/`example`, not in what the option *is*.
  mergeRendered = lib.foldl' (acc: opts: acc // opts) { };

  allNixos = mergeRendered (map (cfg: optionsOf cfg.options) (lib.attrValues nixosConfigs));
  allDarwin = mergeRendered (map (cfg: optionsOf cfg.options) (lib.attrValues darwinConfigs));
  allHome = mergeRendered (map (t: optionsOf t.options) homeTargets);

  # Keep the sources disjoint: nixvim is thousands of options and gets its own
  # source, so drop that subtree from the home-manager one.
  home = lib.filterAttrs (name: _: !lib.hasPrefix nixvimPrefix name) allHome;
  nixvim = lib.filterAttrs (name: _: lib.hasPrefix nixvimPrefix name) allHome;

  # Everything this repo declares itself, across all three classes.
  dendritic = lib.filterAttrs (_: isOurs) (allNixos // allDarwin // allHome);

  mkSource =
    key: options:
    pkgs.runCommand "searchix-source-${key}" { } ''
      mkdir -p "$out"
      cp ${builtins.toFile "options.json" (builtins.unsafeDiscardStringContext (builtins.toJSON (withLinkableDeclarations options)))} "$out/options.json"
      printf '%s' ${lib.escapeShellArg revision} > "$out/revision"
    '';

  sources = lib.mapAttrs mkSource {
    inherit
      dendritic
      nixvim
      home
      ;
    nixos = allNixos;
    darwin = allDarwin;
  };

  # What the searchix aspect needs to describe each source in its config. `key`
  # must stay URL-safe (searchix enforces [a-z0-9_-]*).
  meta = {
    dendritic = {
      key = "dendritic";
      name = "Dendritic";
      order = 0;
      source = sources.dendritic;
    };
    nixos = {
      key = "nixos";
      name = "NixOS (${toString (lib.length nixosHosts)} hosts)";
      order = 1;
      source = sources.nixos;
    };
    home-manager = {
      key = "home-manager";
      name = "Home Manager (${toString (lib.length homeTargets)} users)";
      order = 2;
      source = sources.home;
    };
    nixvim = {
      key = "nixvim";
      name = "Nixvim";
      order = 3;
      source = sources.nixvim;
    };
    darwin = {
      key = "darwin";
      name = "nix-darwin (${toString (lib.length darwinHosts)} hosts)";
      order = 4;
      source = sources.darwin;
    };
  };

  # Served as a whole by nginx: one directory per source key.
  tree = pkgs.runCommand "searchix-option-sources" { } (
    lib.concatStringsSep "\n" (
      [ "mkdir -p $out" ] ++ lib.mapAttrsToList (_: src: "ln -s ${src.source} $out/${src.key}") meta
    )
  );
in
{
  inherit
    meta
    tree
    repo
    revision
    ;
}
