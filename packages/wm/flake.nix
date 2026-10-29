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
				# overlays = [
				#	(final: _prev: pinned.lib.genAttrs pinnedNames (name:
				#		final.callPackage
				#			(pinned.lib.head (pinned.lib.splitString ":" pinned.${name}.meta.position))
				#			{}))
				# ];
			};
			quickshell = pinned.quickshell.overrideAttrs (old: {
				buildInputs = (old.buildInputs or []) ++ (with pinned.kdePackages; [
					qt5compat
					qtpositioning
				]);
			});
		in {
			packages = {
				default = pkgs.symlinkJoin {
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
			};
		});
}
