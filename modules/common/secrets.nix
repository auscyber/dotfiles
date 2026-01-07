{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.secrets;
in
{
  options.auscybernix.secrets = {
    enable = lib.mkEnableOption "Enable sops integration for managing secrets.";
  };
  config = lib.mkIf cfg.enable {

    age.secrets.github_token = {
      rekeyFile = ./github_token.age;
#      intermediary = true;
    };
    age.rekey = {
	generatedSecretsDir = ../../secrets/generated;
      agePlugins = with pkgs; [
      ] ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.age-plugin-1p ];
      # Obtain this using `ssh-keyscan` or by looking it up in your ~/.ssh/known_hosts
      # The path to the master identity used for decryption. See the option's description for more information.
      masterIdentities = [
        #        ./main.pub
        #        {
        #          git
        #        }
        #        ./publickey.txt
        #        "age1se1qf48z2tlp6ua8hpyg7vypm0dw8z8nmgusell62r8vpyufemre6escazv5f2"
		{
		identity = ./age-yubikey.pub;
		pubkey = "age1yubikey1qv6zc6sjz4klkjxnnt2sv8ptlcjtmhphduu4rrqjuw88jn2nftuu6ep0kr3";

		}
        {
          identity = ./main.pub; # Private key
          pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"; # Public key
        }
      ];
      #masterIdentities = [ "/home/myuser/master-key" ]; # External master key
      #masterIdentities = [
      #  # It is possible to specify an identity using the following alternate syntax,
      #  # this can be used to avoid unecessary prompts during encryption.
      #  {
      #    identity = "/home/myuser/master-key.age"; # Password protected external master key
      #    pubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq"; # Specify the public key explicitly
      #  }
      #];
      storageMode = "local";
      # Choose a directory to store the rekeyed secrets for this host.
      # This cannot be shared with other hosts. Please refer to this path
      # from your flake's root directory and not by a direct path literal like ./secrets

    };
  };
}
