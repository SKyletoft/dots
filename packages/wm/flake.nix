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
			nixosModules.hyprland = { lib, pkgs, ... }: {
				environment = {
					systemPackages = with pkgs; [
						hyprpaper
						hyprsunset
						hypridle
						hyprpolkitagent
						playerctl
						grim
						wl-clipboard
						imagemagick
						swappy
						swaylock-effects
					] ++ [ quickshell ];
				};

				security = {
					polkit = {
						enable = true;
						# Passwordless power management for local wheel users.
						# Needed because Hyprland/quickshell currently escape the logind
						# session cgroup (they sit in the root cgroup instead of
						# session-*.scope), so polkit sees their PowerOff/Reboot/Suspend
						# requests as coming from an inactive session
						# (auth_admin_keep) and logind fails with "interactive
						# authentication has not been enabled by the calling program".
						# Keyed on user/group instead of subject.active/local until
						# session tracking is fixed (e.g. via uwsm-managed Hyprland).
						extraConfig = ''
polkit.addRule(function(action, subject) {
	var powerActions = [
		"org.freedesktop.login1.power-off",
		"org.freedesktop.login1.power-off-multiple-sessions",
		"org.freedesktop.login1.power-off-ignore-inhibit",
		"org.freedesktop.login1.reboot",
		"org.freedesktop.login1.reboot-multiple-sessions",
		"org.freedesktop.login1.reboot-ignore-inhibit",
		"org.freedesktop.login1.suspend",
		"org.freedesktop.login1.suspend-multiple-sessions",
		"org.freedesktop.login1.suspend-ignore-inhibit",
		"org.freedesktop.login1.hibernate",
		"org.freedesktop.login1.hibernate-multiple-sessions",
		"org.freedesktop.login1.hibernate-ignore-inhibit",
		"org.freedesktop.login1.halt",
		"org.freedesktop.login1.halt-multiple-sessions",
		"org.freedesktop.login1.halt-ignore-inhibit",
		"org.freedesktop.login1.set-wall-message",
		"org.freedesktop.login1.set-reboot-to-firmware-setup",
		"org.freedesktop.login1.set-reboot-parameter"
	];
	if (powerActions.indexOf(action.id) >= 0 && subject.isInGroup("wheel")) {
		return polkit.Result.YES;
	}
});'';
					};
				};

				programs.hyprland = {
					enable = true;
					package = pkgs.hyprland;
				};
			};
		});
}
