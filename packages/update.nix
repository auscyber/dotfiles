{
config, inputs,...
}:
{

perSystem = {pkgs,...}:
{

apps.update = {
type = "app";

program = pkgs.writeShellApplication {
 name = "update";
 runtimeInputs = with pkgs; [
 nvfetcher
 ];

text = ''
nvfetcher
	 '';

};

};
}
