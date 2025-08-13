{ config, pkgs, lib, nixGL, waylandSupport, windowsFonts, nativeBuild, ... }:

{
	nix.extraOptions = ''
		experimental-features = nix-command flakes
	'';

	networking.hostName = "persephone";
	time.timeZone = "Europe/Stockholm";

	powerManagement.powertop.enable = true;

	# users.users.u3836 = {
	#	description = "Samuel Kyletoft";
	#	home = "/home/u3836";
	#	isNormalUser = true;
	#	extraGroups = [
	#		"wheel" # Enable ‘sudo’ for the user.
	#		"networkmanager"
	#		"libvirtd"
	#		"dialout"
	#		"docker"
	#		"vboxusers"
	#		"video"
	#	];
	#	shell = pkgs.bash;
	# };

	environment.systemPackages = with pkgs; [
		neovim
		git
		setup-system
		update-keys
		update-system
	];

	system.stateVersion = "21.11";
}
