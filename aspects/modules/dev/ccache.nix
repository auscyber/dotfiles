{

  den.aspects.ccache = {
    provides.auspc.nixos = {
      auscybernix.nix.ccache.env.MAX_SIZE = "15G";
    };
    nixos = {
      auscybernix.nix.ccache = {
        enable = true;
      };

    };

  };
}
