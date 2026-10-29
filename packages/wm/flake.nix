{
	description = "The pinned Hyprland and Quickshell stack";
	inputs = {
		nixpkgs.url     = "github:NixOS/nixpkgs/8825bebf6324e0579d012936eff73379af284b6d";
		unstable.url    = "github:NixOS/nixpkgs/nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, unstable, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
		let
			pinned = import nixpkgs { inherit system; };
			pinnedNames = [
				"hyprland"
				"hyprpaper"
				"hyprsunset"
				"hypridle"
				"hyprpolkitagent"
				"playerctl"
				"grim"
				"wl-clipboard"
				"swappy"
				"swaylock-effects"
			];
			pkgs = import unstable {
				inherit system;
				overlays = [
					(final: _prev: pinned.lib.genAttrs pinnedNames (name:
						final.callPackage
							(pinned.lib.head (pinned.lib.splitString ":" pinned.${name}.meta.position))
							{}))
				];
			};
			hyprland = pkgs.hyprland;
			quickshell = pinned.quickshell.overrideAttrs (old: {
				buildInputs = (old.buildInputs or []) ++ (with pinned.kdePackages; [
					qt5compat
					qtpositioning
				]);
			});
			utilities = pkgs.symlinkJoin {
				name = "hyprland-utilities";
				paths = with pkgs; [
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
			everything = pkgs.symlinkJoin {
				name = "hyprland-everything";
				paths = [
					hyprland
					utilities
				];
			};
		in {
			packages = {
				default    = everything;
				everything = everything;
				hyprland   = hyprland;
				utilities  = utilities;
			};
		});
}
