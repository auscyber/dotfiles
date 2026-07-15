{
  lib,
  pkgs,
  inputs,
  # Which evaluated configurations each option source is generated from.
  #
  # These must be configurations that do NOT themselves enable the searchix
  # aspect: the docs are derived from `self.<class>Configurations.<host>.options`,
  # so pointing this at the host that runs searchix would make that host's
  # module evaluation depend on its own option tree (infinite recursion).
  nixosHost ? "auspc",
  darwinHost ? "Ivys-MacBook-Pro",
  homeHost ? "auspc",
  homeUser ? "auscyber",
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

  nixosOptions = self.nixosConfigurations.${nixosHost}.options;
  darwinOptions = self.darwinConfigurations.${darwinHost}.options;

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
  hmOption = self.nixosConfigurations.${homeHost}.options.home-manager.users;
  hmPayload = hmOption.type.nestedTypes.elemType.functor.payload;
  hmDefinitions = builtins.filter (m: m != null) (
    map (def: def.${homeUser} or null) hmOption.definitions
  );
  homeOptions =
    (lib.evalModules {
      inherit (hmPayload) class;
      specialArgs = hmPayload.specialArgs // {
        name = homeUser;
      };
      modules = hmPayload.modules ++ hmDefinitions ++ [ { _module.args.name = homeUser; } ];
    }).options;

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

  allNixos = optionsOf nixosOptions;
  allDarwin = optionsOf darwinOptions;
  allHome = optionsOf homeOptions;

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
      name = "NixOS (${nixosHost})";
      order = 1;
      source = sources.nixos;
    };
    home-manager = {
      key = "home-manager";
      name = "Home Manager (${homeUser}@${homeHost})";
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
      name = "nix-darwin (${darwinHost})";
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
