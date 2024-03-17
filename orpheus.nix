{ config, pkgs, lib, ... }:

let update-keys = pkgs.callPackage ./packages/update-keys.nix {};
	update-system = pkgs.callPackage ./packages/update-system.nix {};
in {
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
			substituters = [
				"https://nix-community.cachix.org"
				"https://cache.nixos.org/"
			];
			trusted-public-keys = [
				"nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
				"nix.u3836.se:t7H/bFWi14aBFYPE5A00eEQawd7Ssl/fXbq/2C+Bsrs="
			];
		};
		gc = {
			automatic = true;
			dates = "weekly";
			options = "--delete-older-than 30d";
		};
		extraOptions = ''
			min-free = ${toString (1024 * 1024 * 1024)}
			experimental-features = nix-command flakes
		'';
	};

	boot = {
		supportedFilesystems = [ "exfat" ];
		binfmt.emulatedSystems = [ "x86_64-linux" ];
		loader.raspberryPi.firmwareConfig = ''
			gpu_mem=192
			dtparam=audio=on
		'';
	};

	networking = {
		hostName = "orpheus";
		firewall = {
			enable = true;
			allowedTCPPorts = [ 53 1401 ]; # Mullvad
			allowedUDPPorts = [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 51820 ]; # Mullvad
		};
	};

	fileSystems = {
		"/" = {
			device = "/dev/disk/by-label/NIXOS_SD";
			fsType = "ext4";
			options = [ "noatime" ];
		};
	};

	documentation = {
		dev.enable = false;
		man.generateCaches = false;
	};

	environment.systemPackages = with pkgs; [
		micro
		git
		man-pages
		man-pages-posix

		update-system
		update-keys
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
		# raspberry-pi."4".fkms-3d.enable = true;
		opengl = {
			enable = true;
			setLdLibraryPath = true;
			package = pkgs.mesa_drivers;
		};
		pulseaudio.enable = true;
	};
	powerManagement.cpuFreqGovernor = "ondemand";
	sound.enable = true;

	services = {
		xserver.enable = false;
		openssh = {
			enable = true;
			settings.PasswordAuthentication = false;
		};
		earlyoom.enable = true;
		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};
		mullvad-vpn.enable = true;
		transmission = {
			enable = true;
			openRPCPort = true;
			settings = {
				rpc-bind-address = "0.0.0.0";
				rpc-whitelist = "127.0.0.1,192.168.*.*";
			};
		};
		cron = {
			enable = true;
			systemCronJobs =
				let update-keys = "${update-keys}/bin/update-keys";
				in [
					("* * * * * u3836 "
					 + "${pkgs.neofetch}/bin/neofetch > /tmp/eurydice-status; "
					 + "SYSTEMD_COLORS=true systemctl status mullvad-daemon | head -n3 >> /tmp/eurydice-status; "
					 + "curl https://am.i.mullvad.net/connected >> /tmp/eurydice-status; "
					)
					("*/05 * * * * u3836 ${update-keys} SKyletoft")
				];
		};
	};

	systemd.services.autoUpdateConfig = {
		wantedBy = [ "default.target" ];
		serviceConfig = {
			Type = "simple";
			ExecStart = "${update-system}/bin/update-system";
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

	system.stateVersion = "21.11";
}
