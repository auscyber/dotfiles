# The public half of the SSH key whose private key lives in the user's
# 1Password vault and never touches disk.
#
# Plain data, `_`-prefixed so the import tree skips it: ./main-ssh-key.nix
# wires it into hosts, and ../nixos/iso.nix needs it outside any host scope.
"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"
