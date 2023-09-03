{ config, pkgs, lib, ... }:

let
	update-website = pkgs.writeShellScriptBin "update-website" ''
		cd /var/www

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/samuel.kyletoft.se tmp
		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/valkompass_2022 tmp/valkompass
		rm -rf \
			tmp/.git \
			tmp/.gitignore \
			tmp/LICENSE \
			tmp/valkompass/.git \
			tmp/valkompass/.gitignore \
			tmp/valkompass/LICENSE \
			samuel.kyletoft.se
		mv tmp samuel.kyletoft.se

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/u3836.se tmp
		rm -rf \
			tmp/.git \
			tmp/.gitignore \
			tmp/LICENSE \
			u3836.se
		mv tmp u3836.se

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/secure-passwords tmp
		rm -rf \
			tmp/.git
			tmp/LICENSE \
			secure-passwords
		mv tmp secure-passwords

		${pkgs.git}/bin/git clone \
			https://github.com/liamjardine/liamjardine.github.io tmp
		rm -rf \
			tmp/.git \
			liamjardine.se
		mv tmp liamjardine.se
	'';
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
		micro
		git
		man-pages
		man-pages-posix
		hugo
	];

	users.users = {
		u3836 = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		maky.isNormalUser = true;
		liam.isNormalUser = true;
		enaya.isNormalUser = true;
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
		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};
		mullvad-vpn.enable = true;
		jellyfin = {
			enable = true;
			openFirewall = true;
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
				("* * * * * root ${update-website}/bin/update-website")
			];
		};
		nix-serve = {
			enable = true;
			secretKeyFile = "/var/cache-priv-key.pem";
			bindAddress = "127.0.0.1";
			port = 5000;
		};
		nginx = {
			enable = true;
			recommendedProxySettings = true;
			recommendedTlsSettings = true;
			recommendedGzipSettings = true;
			recommendedOptimisation = true;
			statusPage = true;
			virtualHosts = {
				"kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/kyletoft.se";
				};
				"samuel.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					default = true;
					root = "/var/www/samuel.kyletoft.se";
				};
				"u3836.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/u3836.se";
				};
				"marie.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/marie.kyletoft.se";
				};
				"www.liamjardine.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/liamjardine.se";
				};
				"liamjardine.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/liamjardine.se";
				};
				"dhack.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/dhack.kyletoft.se";
				};
				"passwords.u3836.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/secure-passwords";
				};
				"cflisp.u3836.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/cflisp.kyletoft.se";
				};
				"termshare.u3836.se" = {
					addSSL = true;
					enableACME = true;
					locations."/".proxyPass = "http://127.0.0.1:12825";
				};
				"jellyfin.u3836.se" = {
					addSSL = true;
					enableACME = true;
					locations."/".proxyPass = "http://127.0.0.1:8096";
				};
				"nix.u3836.se" = {
					addSSL = true;
					enableACME = true;
					locations."/".extraConfig = ''
						proxy_pass http://127.0.0.1:5000;
						proxy_set_header Host $host;
						proxy_set_header X-Real-IP $remote_addr;
						proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
					'';	
				};
			};
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
		acme = {
			acceptTerms = true;
			defaults.email = "samuel+acme@kyletoft.se";
		};
	};

	system.stateVersion = "21.11";
}
