{
inputs,...
}:

{


perSystem = { system, pkgs, ... }: {

	apps.fetch = {
	type = "app";
	program = inputs.nvfetcher.packages."${system}".default;
	};
};
}
