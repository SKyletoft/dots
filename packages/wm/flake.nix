{
	description = "The pinned Hyprland and Quickshell stack";
	inputs = {
		nixpkgs.url     = "github:NixOS/nixpkgs/8825bebf6324e0579d012936eff73379af284b6d";
		unstable.url    = "github:NixOS/nixpkgs/nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, unstable, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
		let
			graphics = import unstable { inherit system; };
			pkgs = import nixpkgs {
				inherit system;
				overlays = [
					(_final: _prev: {
						mesa        = graphics.mesa;
						libgbm      = graphics.libgbm;
						libglvnd    = graphics.libglvnd;
						libGL       = graphics.libGL;
						libdrm      = graphics.libdrm;
						# qt6         = graphics.qt6;
						# gtk4        = graphics.gtk4;
						# libadwaita  = graphics.libadwaita;
						# kdePackages = graphics.kdePackages;
					})
				];
			};
			hyprland = pkgs.hyprland;
			quickshell = pkgs.quickshell.overrideAttrs (old: {
				buildInputs = (old.buildInputs or []) ++ (with pkgs.kdePackages; [
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
