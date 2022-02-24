# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
	hardware = {
		opengl.enable = true;
		nvidia = {
			modesetting.enable = true;
			package = pkgs.linuxPackages_zen.nvidia_x11;
		};
	};

	fileSystems."/mnt/SDD" = {
		device = "/dev/sdd1";
		fsType = "ntfs";
		options = [
			"allow_other"
			"x-systemd.automount"
		];
	};

	networking = {
		hostName = "skyletoft-ii-nix"; # Define your hostname.

		# wireless.enable = true;  # Enables wireless support via wpa_supplicant.
		# useDHCP = false;
		networkmanager.enable = true;
		interfaces = {
			enp0s31f6.useDHCP = false; # WiFi card
			# wlp0s20f0u1.useDHCP = false; # USB WiFi dongle
		};

		# Configure network proxy if necessary
		# proxy = {
		# 	default = "http://user:password@proxy:port/";
		# 	noProxy = "127.0.0.1,localhost,internal.domain";
		# };

		# Open ports in the firewall.
		# firewall = {
		# 	allowedTCPPorts = [ ... ];
		# 	allowedUDPPorts = [ ... ];
		# };
		# Or disable the firewall altogether.
		# firewall.enable = false;
	};

	services.xserver.videoDrivers = [ "nvidia" ];
}
