{ pkgs, ... }:
{
  home.services."basic" = {
    process.argv = [
      "${pkgs.mpd}/bin/mpd"
      "--no-daemon"
    ];
  };

  nmt.script = ''
    assertFileContent home-files/.config/systemd/user/basic.service ${./basic.service}
  '';
}
{ pkgs, ... }:
{
  home.services.demo = {
    process.argv = [
      "${pkgs.coreutils}/bin/echo"
      "hello"
    ];
  };

  nmt.script = ''
    assertFileExists home-files/.config/systemd/user/demo.service
    assertFileContains home-files/.config/systemd/user/demo.service '/bin/echo'
    assertFileContains home-files/.config/systemd/user/demo.service 'WantedBy=default.target'
  '';
}
