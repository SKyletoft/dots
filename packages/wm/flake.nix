{
	description = "The pinned Hyprland and Quickshell stack";
	inputs = {
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
		{
			nixosModules.hyprland = { lib, pkgs, ... }: {
				nixpkgs.overlays = [
					(final: prev: {
						quickshell = prev.quickshell.overrideAttrs (old: {
							buildInputs = (old.buildInputs or []) ++ (with prev.kdePackages; [
								qt5compat
								qtpositioning
							]);
						});
					})
				];
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
						quickshell
					];
				};

				security = {
					polkit = {
						enable = true;
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
