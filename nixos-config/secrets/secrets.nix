let
  auscyber = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H willp@outlook.com.au";
  auspc = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPyXyBMVHaVmR3f5gI3pQ+VWdMjHKPI9yR4/trIV+W/D root@auspc";
in
{
  "spotify_api.age".publicKeys = [
    auscyber
    auspc
  ];
}
