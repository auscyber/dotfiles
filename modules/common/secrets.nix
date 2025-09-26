{
  sops.defaultSopsFile = ../../secrets/default.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.secrets.github_token = { };
  sops.secrets.password = { };
}
