{
  lib,
  den,
  ...
}:
# The household service instance type, built once and read as `serviceLibs`
# rather than path-imported per call site.
{
  _module.args.serviceLibs.mkHouseholdServiceType = import ../../lib/service-type.nix {
    inherit lib den;
    capability = den.lib.capability;
  };
}
