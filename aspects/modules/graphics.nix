{ lib, ... }:
{

  den.schema.host = {
    options.gpu = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Active GPU support for gaming and graphics.";
    };
  };
}
