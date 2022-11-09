{ config, pkgs, lib, ... }:

{
	imports = [
		<nixos-hardware/raspberry-pi/4>
	];

	nixpkgs = {
		config.allowUnfree = true;
		overlays = [
			(final: prev: {
				mullvad-vpn = prev.mullvad;
			})
		];
	};
	nix = {
		settings.auto-optimise-store = true;
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
	};

	networking = {
		hostName = "eurydice";
		firewall = {
			enable = true;
			allowedTCPPorts =
				[ 80 443 8000 8080 12825 ] # Development
				++ [ 53 1401 ]; # Mullvad
			allowedUDPPorts =
				[ 80 443 8000 8080 12825 ] # Development
				++ [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 51820 ]; # Mullvad
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

	users.users = {
		u3836 = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		maky.isNormalUser = true;
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
		adguardhome.enable = false;
		invidious = {
			enable = true;
			nginx.enable = true;
			domain = "yt.kyletoft.se";
		};
		mullvad-vpn.enable = true;
		jellyfin = {
			enable = true;
			openFirewall = true;
		};
		nginx = {
			enable = true;
			recommendedProxySettings = true;
			recommendedTlsSettings = true;
			recommendedGzipSettings = true;
			recommendedOptimisation = true;
			virtualHosts = {
				"kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					default = true;
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
					addSSL = true;
					enableACME = true;
					locations."/".proxyPass = "http://127.0.0.1:12825";
				};
				"jellyfin.kyletoft.se" = {
					addSSL = true;
					enableACME = true;
					locations."/".proxyPass = "http://127.0.0.1:8096";
				};
			};
		};
	};

	programs.bash.shellInit = ''
		[[ $- == *i* ]] || return
		${pkgs.neofetch}/bin/neofetch --disable packages
		SYSTEMD_COLORS=true systemctl status nginx | head -n3
		SYSTEMD_COLORS=true systemctl status jellyfin | head -n3
		SYSTEMD_COLORS=true systemctl status mullvad-daemon | head -n3
		SYSTEMD_COLORS=true systemctl status invidious | head -n3
	'';

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
