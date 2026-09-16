# The NUR inputs, owned by one aspect that every consumer includes.
#
# They used to be declared in whichever browser aspect happened to reach them:
# `nur` on ../programs/browsers/zen.nix and `my-nur` as a file-level `ff.` in
# ../programs/browsers/helium.nix. The conversion to aspect-level inputs dropped
# the latter, and nothing noticed -- helium guards its use with
# `inputs ? my-nur`, so it degraded silently, while zen dereferences it directly
# and only failed once a host evaluation reached that line.
#
# One declaration with one owner, included by both, so the input exists exactly
# when a consumer does.
{ den, ... }: {
  den.aspects.nur = {
    layers = [ "gui" ];

    inputs.nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    # auscyber's own NUR: packages that are not in nixpkgs or upstream NUR.
    inputs.my-nur = {
      url = "github:auscyber/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
