{ den, lib, ... }:
let
  fontPkgs =
    pkgs:
    with pkgs;
    map (x: nerd-fonts.${x}) [
      "fira-code"
      "inconsolata"
      "hasklug"
      "roboto-mono"
    ];
in
{
  den.aspects.fonts = {
    # Two independent gates instead of the host+user intersection used by
    # `gui = { ... }` (see roles.nix). This way a standalone home-manager
    # target with the `gui` role on its user — and no host record at all —
    # still gets nerd fonts installed at the HM layer.
    includes = [
      # System-level fonts: fires when the host advertises `gui`.
      (den.lib.policy.when
        ({ host ? { }, ... }: lib.elem "gui" (host.roles or [ ]))
        {
          os =
            { pkgs, ... }:
            {
              fonts.packages = fontPkgs pkgs;
            };
        })

      # User-level fonts: fires when the user advertises `gui`, regardless
      # of whether a host is present. This is what makes HM-only targets work.
      (den.lib.policy.when
        ({ user ? { }, ... }: lib.elem "gui" (user.roles or [ ]))
        {
          homeManager =
            { pkgs, ... }:
            {
              home.packages = fontPkgs pkgs;
              fonts.fontconfig.enable = true;
            };
        })
    ];
  };

  den.schema.host.includes = [ den.aspects.fonts ];
  den.schema.user.includes = [ den.aspects.fonts ];
}
