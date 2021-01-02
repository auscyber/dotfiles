self: super:
with super;
rec{
  powercord = super.discord-canary.overrideAttrs (attrs: {
      powercord = fetchFromGitHub {
	  repo = "powercord";
	  owner = "powercord-org";
	  rev = "55d123f7325a25e91551635f7f8a878a028cf1e8";
	  sha256= "1x429n2kwwc4z0fhccbafv2wnnbwjl5c99kp9k8yl029n19zfmwh";
      };
      buildInputs = super.discord-canary.buildInputs ++ [ nodejs git ];
      installPhase = super.discord-canary.installPhase + ''
	  cd $powercord
	  echo $powercord > $out/powercord
#	  npm i
#	  npm run plug <<< $out
      '';
  });
}
