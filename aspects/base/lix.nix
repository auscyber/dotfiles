{
  inputs,
  lib,
  ...
}:
{
  ff = {
    izlix.url = "github:isabelroses/izlix";
    izlix.inputs.nixpkgs.follows = "nixpkgs";

    ivylix.url = "github:auscyber/ivylix";
    ivylix.inputs.nixpkgs.follows = "nixpkgs";
    ivylix.inputs.izlix.follows = "izlix";
    ivylix.inputs.ivixlib.follows = "ivixlib";
    ivixlib.url = "github:auscyber/ivixlib";
    ivixlib.inputs.nixpkgs.follows = "nixpkgs";

  };

  # The entire Lix build lives in the `ivylix` flake (sourced via nvfetcher through
  # the shared `ivixlib` mechanism). We reuse its scope *builder* rather than its
  # pre-built packages, so the whole scope compiles against ONE Lix — ccache-wrapped
  # on hosts that run the ccache aspect. (Injecting the flat `ivylix.packages` and
  # then ccache-wrapping only `pkgs.lix` would build Lix twice: the wrapped one for
  # `nix.package`, and the plain one `nil`/etc. still depend on.)
  den.aspects.lix.os =
    { pkgs, config, ... }:
    let
      # ccache is host state (declared by the ccache aspect). attrByPath so the
      # option being undeclared — no ccache aspect here — reads as `false`.
      ccacheEnabled = lib.attrByPath [ "auscybernix" "nix" "ccache" "enable" ] false config;

      # Normal clang stdenv; ccache-wrapped when ccache is on this host, so the
      # (expensive) Lix compiles go through the shared cache. From the host `pkgs`
      # so the ccacheWrapper the ccache aspect configures is the one used.
      cxxStdenv =
        if ccacheEnabled then
          pkgs.ccacheStdenv.override { stdenv = pkgs.clangStdenv; }
        else
          pkgs.clangStdenv;

      # Build the scope from an independent nixpkgs fixpoint: the scope's tools are
      # `<pkg>.override { nix = self.lix; }`, so routing them back into the host
      # `pkgs` (below) from that *same* pkgs would self-reference. A separate import
      # makes `nil`/`colmena`/… resolve to stock packages, breaking the cycle.
      basePkgs = import inputs.nixpkgs { inherit (pkgs.stdenv.hostPlatform) system; };

      scope = inputs.ivylix.mkScope {
        pkgs = basePkgs;
        inherit cxxStdenv;
      };
    in
    {
      # One Lix; every lix-adjacent tool on the host resolves to the scope build
      # (all sharing that single `scope.lix`) rather than the stock one.
      nixpkgs.overlays = lib.mkAfter [
        (_final: _prev: {
          inherit (scope)
            lix
            nil
            nix-eval-jobs
            nix-fast-build
            nixpkgs-review
            nixpkgs-reviewFull
            colmena
            ;
        })
      ];
      nix.package = scope.lix;
    };
}
