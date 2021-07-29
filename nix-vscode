#! /usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = sh $ do
  homedir <- home
  subdir <- ls $ homedir </> ".vscode-server/bin/"
  let nodepath = subdir </> "node"
  badnode <- isNotSymbolicLink nodepath
  if badnode
    then do
      mv nodepath (subdir </> "node_backup")
      symlink "/run/current-system/sw/bin/node" nodepath
      echo ("Fixed " <> repr subdir)
    else do
      echo ("Already fixed " <> repr subdir)
