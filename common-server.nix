{ config, pkgs, lib, ... }:

let
	setup-system = pkgs.callPackage ./packages/setup-system.nix {};
	update-system = pkgs.callPackage ./packages/update-system.nix {};
	update-keys = pkgs.callPackage ./packages/update-keys.nix {};
in {
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

	boot.supportedFilesystems.exfat = true;

	networking = {
		firewall = {
			enable = true;
			allowedTCPPorts =
				[ 80 443 8000 8080 12825 ] # Development
				++ [ 53 1401 ]; # Mullvad
			allowedUDPPorts =
				[ 80 443 8000 8080 12825 ] # Development
				++ [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 51820 ]; # Mullvad
		};
	};

	virtualisation.docker.enable = true;

	documentation = {
		dev.enable = false;
		man.generateCaches = false;
	};

	environment.systemPackages = with pkgs; [
		micro
		git
		man-pages
		man-pages-posix
		update-keys
		update-system
		setup-system
		nix-output-monitor
	];

	users.users = {
		u3836 = {
			isNormalUser = true;
			extraGroups = [ "wheel" "jellyfin" "transmission" "docker" ];
		};
		root.openssh.authorizedKeys.keys = [
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNXgCDGyWMeQBTCloSMMEASjOLjvIOcx+HazUOrS3OR"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGzlJyY+rRehRff2s9aL8XtA6flDCqnLBz0AN7q50ivU"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJB8NV4FJc7y9gBDTBtfenUSm97Hn1eFRjmwMnILB737"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOKctWKuIyHNzEe6hPt/1B/elI+0hvXKjLgUS5Kiz15o"
		];
	};

	powerManagement.cpuFreqGovernor = "ondemand";

	services = {
		xserver.enable = false;
		openssh = {
			enable = true;
			settings = {
				PasswordAuthentication = false;
				X11Forwarding = true;
			};
		};
		earlyoom.enable = true;
		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};
		mullvad-vpn.enable = true;
		cron = {
			enable = true;
			systemCronJobs = [
				("*/05 * * * * u3836 ${update-keys}/bin/update-keys SKyletoft")
				("*/05 * * * * root  ${update-system}/bin/update-system")
			];
		};
		lorri = {
			enable = true;
			package = pkgs.lorri;
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
