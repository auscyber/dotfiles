#!/bin/sh
nix () {
    nix --experimental-features 'nix-command flakes' $@
}
set -e
if test -z "$1"
then
    echo -e '\033[38;5;160m'Error: '\033[0m'System not supplied!
    exit 1
fi
echo -e '\033[38;5;214m'System: '\033[0m'"$1"
nix build .#homeConfigurations."$1".activationPackage
./result/activate
