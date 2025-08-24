let
  auscyber = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H willp@outlook.com.au";
  auspc = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPyXyBMVHaVmR3f5gI3pQ+VWdMjHKPI9yR4/trIV+W/D root@auspc";
  mac = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDWLBfISIzlyIXewHmETOnkiPI09bpqU1oywBvLoclWaettdNaP84NGmCRvADLc3DkWAzQo/WB4nVlEEQXD7kvcptnUC0ufyhH1su9yQYKYLRu01E9KzUmeIWrGTd6x+EcQ12qP4xaY3EjgJyLdZjZj0dlBtpubRWmlLiZ8Dz832hBsx/wL5F/4jgirwBJxvmnNDDbKRylwcMxwTIoyU1hzTZc5br9j4ZELomDLR6IChByLWLooZzrI8Hrws3DUdUhyD+mdrfhqOnL0lb/xlsI9wGJhoxaT3FkkKHb//HMRWth5RjO/ygGG1L/HMk9oyvfbF6oAX9l5b65TKLdMkePtsA+kyjfrjyGvQyfpv+HQi41yrMXzEeO5uY2vNQqgDuDrbF5jhyEJXDO7aERZVeByOXKCwfIb5Iv8peGkweL/ftQWnpi9ulq9SP3M9Xx1pdohUsuO51fIsnJnYE9tY6OOFvT4zbajJzkVRPIXNFV0ueg1M+e51uByICWCgzAnWJk=";
in
{
  "spotify_api.age".publicKeys = [
    auscyber
    auspc
    mac
  ];
  "smb_secrets.age".publicKeys = [
    auscyber
    auspc
    mac
  ];
  "password.age".publicKeys = [
    auspc
    mac
    auscyber
  ];
  "pia_vpn.age".publicKeys = [
    auspc
    mac
    auscyber
  ];
  "pia_privatekey.age".publicKeys = [
    auspc
    mac
    auscyber
  ];
}
