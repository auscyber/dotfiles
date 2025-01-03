#!/bin/sh
git
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
  sh -s -- install
nix run nix-darwin -- switch --flake ./nixos-config
darwin-rebuild switch --flake ~/.nixos-config
