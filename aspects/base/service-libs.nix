{
  lib,
  den,
  inputs,
  capability,
  ...
}:
# THE SERVICE LIBRARIES, EVALUATED ONCE AND READ FROM A LIB SCOPE.
#
# ./service.nix used to `import ../../lib/service-dep-type.nix` TWICE -- once to
# build the type for `den.serviceTypes` and once for `den.services` -- and that
# file in turn imports ../../lib/service-scope.nix, which imports
# ../../lib/service-link.nix. A path import is evaluated per call site and
# shares no thunk with anything else importing the same file, so the whole scope
# graph was built twice per evaluation for no reason.
#
# Declared here instead, so `import-tree` picks the file up automatically and
# every reader takes `serviceLibs` as a module arg: one evaluation for the whole
# flake, and no file hard-codes its position relative to another.
#
# STILL ONE IMPORT, not zero. The bodies remain in `lib/` and are pulled in here
# exactly once; the end state is those bodies living in auto-imported files so
# the path appears nowhere at all. That is a mechanical move of ~1200 lines and
# is deliberately not bundled with a behaviour change -- but this file is the
# only place the paths are written, so the move touches one file rather than
# four.
let
  # The household's nested `services.<n>` instance type. Applied per household
  # (it closes over that household's members), but BUILT once here for the
  # reason this file exists: ../base/household.nix imported the path directly,
  # so the type -- and the capability queries its derived `endpoints` runs --
  # was constructed per call site.
  mkHouseholdServiceType = import ../../lib/service-type.nix { inherit lib den capability; };

  mkServiceType = import ../../lib/service-dep-type.nix {
    inherit lib den;
    genScope = inputs.gen-scope.lib;
    # The inheritance merge over `includes`. gen-algebra exposes it as
    # `lib.record`, and the fold the service type needs is `foldLayersTraced` --
    # plain `foldLayers` refuses the "semilattice-set" strategy by name.
    record = inputs.gen-algebra.lib.record;
  };
in
{
  _module.args.serviceLibs = {
    # `household -> type`, for the household's own nested `services`.
    inherit mkHouseholdServiceType;

    # `registry -> type`. Called with the instance registry for `den.services`,
    # and with `{ }` for `den.serviceTypes` -- a definition resolves nothing, so
    # it needs no registry, and passing the real one there closes a cycle
    # (definitions are what the instances are generated FROM).
    inherit mkServiceType;
  };
}
