{ config, pkgs, lib, ... }:

{
	imports = [];

	nixpkgs = {
		config.allowUnfree = true;
		overlays = [
			(final: prev: {
				mullvad-vpn = prev.mullvad;
			})
		];
	};
	nix = {
		settings = {
			auto-optimise-store = true;
			# post-build-hook = "/home/u3836/dots/upload-to-cache.sh";
			substituters = [
				"https://nix-community.cachix.org"
				"https://cache.nixos.org/"
				"https://nix.u3836.se/"
			];
			trusted-public-keys = [
				"nix.u3836.se:t7H/bFWi14aBFYPE5A00eEQawd7Ssl/fXbq/2C+Bsrs="
				"nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
			];
		};
		gc = {
			# automatic = true;
			# dates = "weekly";
			# options = "--delete-older-than 30d";
		};
		extraOptions = ''
			min-free = ${toString (1024 * 1024 * 1024)}
			experimental-features = nix-command flakes
		'';
	};

	networking = {
		hostName = "orpheus";
		# interfaces.eth0.ipv4.addresses = [ {
		#	address = "192.168.0.202";
		#	prefixLength = 24;
		# } ];
		firewall = {
			enable = true;
			allowedTCPPorts =
				[ 53 1401 ]; # Mullvad
			allowedUDPPorts =
				[ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 51820 ]; # Mullvad
		};
	};

	fileSystems."/" = {
		device = "/dev/disk/by-label/NIXOS_SD";
		fsType = "ext4";
		options = [ "noatime" ];
	};

	# documentation = {
		# dev.enable = true;
		# man.generateCaches = true;
	# };

	environment.systemPackages = with pkgs; [
		micro
		# man-pages
		# man-pages-posix
	];


	users.users = {
		u3836 = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		root.openssh.authorizedKeys.keys = [
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNXgCDGyWMeQBTCloSMMEASjOLjvIOcx+HazUOrS3OR"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGzlJyY+rRehRff2s9aL8XtA6flDCqnLBz0AN7q50ivU"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJB8NV4FJc7y9gBDTBtfenUSm97Hn1eFRjmwMnILB737"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOKctWKuIyHNzEe6hPt/1B/elI+0hvXKjLgUS5Kiz15o"
		];
	};

	hardware = {
		pulseaudio.enable = false;
		raspberry-pi."4".fkms-3d.enable = false;
	};
	powerManagement.cpuFreqGovernor = "ondemand";

	services = {
		xserver.enable = false;
		openssh = {
			enable = true;
			settings.PasswordAuthentication = false;
		};
		earlyoom.enable = true;
		# ananicy = {
			# enable = true;
			# package = pkgs.ananicy-cpp;
		# };
		# mullvad-vpn.enable = true;
		lorri = {
			enable = true;
			package = pkgs.lorri;
		};
		cron = {
			enable = true;
			systemCronJobs = [
				("* * * * * u3836 "
					+ "${pkgs.neofetch}/bin/neofetch > /tmp/eurydice-status "
					+ "&& SYSTEMD_COLORS=true systemctl status nginx | head -n3 >> /tmp/eurydice-status "
					+ "&& SYSTEMD_COLORS=true systemctl status jellyfin | head -n3 >> /tmp/eurydice-status "
					+ "&& SYSTEMD_COLORS=true systemctl status mullvad-daemon | head -n3 >> /tmp/eurydice-status "
				)
			];
		};
	};

	programs = {
		bash.shellInit = ''
			[[ $- == *i* ]] || return
			cat /tmp/eurydice-status
		'';
		ssh.startAgent = true;
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
