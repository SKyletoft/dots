{
	description = "The pinned Hyprland and Quickshell stack";
	inputs = {
		nixpkgs.url     = "github:NixOS/nixpkgs/8825bebf6324e0579d012936eff73379af284b6d";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
		let
			pkgs = import nixpkgs { inherit system; };
			quickshell = pkgs.quickshell.overrideAttrs (old: {
				buildInputs = (old.buildInputs or []) ++ (with pkgs.kdePackages; [
					qt5compat
					qtpositioning
				]);
			});
			everything = pkgs.symlinkJoin {
				name = "hyprland-everything";
				paths = with pkgs; [
					hyprland
					hyprpaper
					hyprsunset
					hypridle
					hyprpolkitagent
					playerctl
					quickshell
					grim
					wl-clipboard
					imagemagick
					swappy
					swaylock-effects
				];
			};
		in {
			packages = {
				default    = everything;
				everything = everything;
			};
		});
}
