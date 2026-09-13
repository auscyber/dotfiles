{ den, ... }:
{
  #patchedInputs.nixpkgs.patches = [ ../patches/nixpkgs/fix-vscode.patch ];

  # Upstream https://github.com/NixOS/nixpkgs/pull/560020: inline-snapshot's
  # test_docs.py is sensitive to formatter versions and flakes on our pin.
  den.aspects.inline-snapshot-fix = {
    overlays = { ... }: {
      inline-snapshot = final: prev: {
        pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
          (pyFinal: pyPrev: {
            inline-snapshot = pyPrev.inline-snapshot.overrideAttrs (old: {
              disabledTestPaths = (old.disabledTestPaths or [ ]) ++ [ "tests/test_docs.py" ];
            });
          })
        ];
      };
    };
  };

  den.schema.host.includes = [ den.aspects.inline-snapshot-fix ];
}
