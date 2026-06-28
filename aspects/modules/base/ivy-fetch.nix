{ den, lib, ... }:
{
  # The overlay providing `pkgs.ivy-fetch` is declared in
  # packages/ivy-fetch/default.nix as `den.aspects.packages.ivy-fetch`. This
  # aspect just wires the resulting binary into the user's home environment
  # and shell startup.
  den.aspects.ivy-fetch = {
    includes = [ den.aspects.packages.ivy-fetch ];

    homeManager =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.ivy-fetch ];

        # Run `fetch -p` (prints the phoebe header) at every interactive shell
        # start. These options are no-ops when the shell isn't enabled.
        programs.fish.interactiveShellInit = lib.mkAfter "fetch -p";
        programs.zsh.initContent = lib.mkAfter "fetch -p";
        programs.nushell.extraConfig = lib.mkAfter "fetch -p";
      };
  };

  den.schema.user.includes = [ den.aspects.ivy-fetch ];
}
