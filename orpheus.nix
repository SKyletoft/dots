{ config, pkgs, lib, ... }:

{
	imports = [
		<nixos-hardware/raspberry-pi/4>
	];

	nixpkgs.config.allowUnfree = true;
	nix = {
		settings.auto-optimise-store = true;
		gc = {
			automatic = true;
			dates = "weekly";
			options = "--delete-older-than 30d";
		};
		extraOptions = ''
			min-free = ${toString (100 * 1024 * 1024)}
			experimental-features = nix-command flakes
		'';
	};

	networking = {
		hostName = "orpheus";
		interfaces.eth0.ipv4.addresses = [ {
			address = "192.168.1.200";
			prefixLength = 24;
		} ];
		defaultGateway = "192.168.1.1";
		nameservers = [ "8.8.8.8" ];
	};

	fileSystems."/" = {
		device = "/dev/disk/by-label/NIXOS_SD";
		fsType = "ext4";
		options = [ "noatime" ];
	};

	documentation = {
		dev.enable = true;
		man.generateCaches = true;
	};

	environment.systemPackages = with pkgs; [
		neovim
		man-pages
		man-pages-posix
	];


	users.users.u3836 = {
		isNormalUser = true;
		extraGroups = [ "wheel" ];
	};

	hardware = {
		pulseaudio.enable = false;
		# Enable GPU acceleration
		raspberry-pi."4".fkms-3d.enable = false;
	};
	powerManagement.cpuFreqGovernor = "ondemand";

	services = {
		xserver.enable = false;
		openssh = {
			enable = true;
			passwordAuthentication = false;
		};
		earlyoom.enable = true;
		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};
	};

	security = {
		sudo.enable = false;
		doas = {
			enable = true;
			extraRules = [{
				users = [ "u3836" ];
				keepEnv = true;
				persist = true;
			}];
		};
	};
}
