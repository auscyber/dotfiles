{
  perSystem = { pkgs, ... }: {
    # We pin to a specific nixpkgs commit for reproducibility.
    # Last updated: 2024-04-29. Check for new commits at https://status.nixos.org.
    devShells.lyrics = pkgs.mkShell {
      packages = [
        (pkgs.python3.withPackages (
          python-pkgs: with python-pkgs; [
            # select Python packages here
            lyricsgenius
            pandas
            requests
          ]
        ))
      ];
    };
  };
}
