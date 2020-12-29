{pkgs, lib, ...}:
let current_song = ./scripts/currentPlay.py;
in
{
  enable = true;
  config = {
      "global/wm" = {
	  margin-top = 5;
	  margin-bottom = 5;

      };
      "bar/mybar" = {
	  monitor = "HDMI-0";
	  modules-right = "playing-song pulseaudio cpu temp memory date poweroff";
	  modules-center = "time";
	  modules-left = "workspaces-xmonad";
	  font-0 = "RobotoMono Nerd Font:style=Medium:size=10;2";
	  font-1 = "RobotoMono Nerd Font:style=Bold:pixelsize=15";
	  enable-ipc = true;
	  foreground = "#FFBD9E";
	  background = "#9591FF";
	  radius = 5.0;
	  tray-position = "right";
	  tray-detached = false;
	  border-top-size = 10;
	  border-bottom-size = 5;
	  border-left-size = 10;
	  border-right-size = 10;
	  line-size=3;
	  height = 25;
	  module-margin = 1;
  
      };
      "module/date" = {
	type = "internal/date";
	date = "  %a %d %b";
	format-foreground = "#FFD1DC";

      };
      "module/time" = {
	  type = "internal/date";
	  date = "%I:%M%P";
      };
      "module/temp" = {
	  type = "internal/temperature";
	  interval = 0.5;
	  format = "<ramp> <label>";
	  hwmon-path = "/sys/devices/platform/coretemp.0/hwmon/temp1_input";
	  base-temperature = 20;
	  warm-temperature = 60;
	  ramp-0 = "";
	  ramp-1 = "";
	  ramp-2 = "";


      };
      "module/cpu" = {
	type = "interval/cpu";
	interval = 0.5;
	format = " <label>";
	format-foreground = "#85FFB7";

      };
      "module/memory" = {
	type = "internal/memory";
	interval = 3;
	label = " %gb_used%/%gb_total%";
	format-foreground = "#C4D1FF";

      };
      "module/playing-song" = {
	type = "custom/script";
	exec = "${current_song}";
	tail = true;
	format-foreground = "#806768";
      };
      "module/workspaces-xmonad" = {
	  type = "custom/script";
	  exec = "tail -F /tmp/.xmonad-workspace-log";
	  tail = true;
	  format-offset = 10;
      };
      "module/battery" = {


      };
      "module/pulseaudio" = {
	 type = "internal/pulseaudio";
      };
  };


}
