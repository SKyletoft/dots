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
			max-free = ${toString (1024 * 1024 * 1024)}
			experimental-features = nix-command flakes
		'';
	};

	boot = {
		supportedFilesystems = [ "exfat" ];
		binfmt.emulatedSystems = [ "x86_64-linux" ];
	};

	networking = {
		hostName = "eurydice";
		firewall = {
			enable = true;
			allowedTCPPorts = [ 80 443 8000 8080 12825 ];
			allowedUDPPorts = [ 80 443 8000 8080 12825 ];
		};
		# interfaces.eth0.ipv4.addresses = [ {
		# 	address = "192.168.0.200";
		# 	prefixLength = 24;
		# } ];
		# defaultGateway = "192.168.0.1";
		# nameservers = [ "8.8.8.8" ];
	};

	fileSystems = {
		"/" = {
			device = "/dev/disk/by-label/NIXOS_SD";
			fsType = "ext4";
			options = [ "noatime" ];
		};
		"/mnt/T7" = {
			device = "/dev/disk/by-label/T7"; # Actual device is randomised for some reason
			fsType = "exfat";
			options = [
				"noatime"
				"allow_other"
				"x-systemd.automount"
			];
		};
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
		shell = pkgs.bash;
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
		nginx = {
			enable = true;
			recommendedProxySettings = true;
			recommendedTlsSettings = true;
			virtualHosts = {
				"kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/kyletoft.se";
				};
				"samuel.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/samuel.kyletoft.se";
				};
				"marie.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/marie.kyletoft.se";
				};
				"dhack.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/dhack.kyletoft.se";
				};
				"cflisp.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/cflisp.kyletoft.se";
				};
				"termshare.kyletoft.se" = {
					enableSSL = true;
					enableACME = true;
					locations."/" = {
						proxyPass = "https://127.0.0.1:12825";
						proxyWebsockets = true;
					};
				};
			};
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
		acme = {
			acceptTerms = true;
			defaults.email = "samuel+acme@kyletoft.se";
		};
	};

	system.stateVersion = "21.11";
}
