{ config, pkgs, lib, nixGL, waylandSupport, windowsFonts, nativeBuild, ... }:

{
	networking.hostName = "persephone";

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

	system.stateVersion = "21.11";
}
