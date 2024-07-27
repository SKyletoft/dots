{ config, pkgs, lib, ... }:

let
	update-system = pkgs.callPackage ./packages/update-system.nix {};
	update-keys = pkgs.callPackage ./packages/update-keys.nix {};
	update-website = pkgs.writeShellScriptBin "update-website" ''
		cd /var/www

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/samuel.kyletoft.se tmp1
		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/valkompass_2022 tmp1/valkompass
		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/cv tmp0
		cd tmp0
		${pkgs.tectonic}/bin/tectonic en.tex
		${pkgs.tectonic}/bin/tectonic sv.tex
		mv en.pdf ../tmp1/cv/en/CV.pdf
		mv sv.pdf ../tmp1/cv/sv/CV.pdf
		cd ..
		rm -rf \
			tmp0
			tmp1/.git \
			tmp1/.gitignore \
			tmp1/LICENSE \
			tmp1/valkompass/.git \
			tmp1/valkompass/.gitignore \
			tmp1/valkompass/LICENSE \
			samuel.kyletoft.se
		mv tmp1 samuel.kyletoft.se

		sleep 15s

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/u3836.se tmp2
		rm -rf \
			tmp2/.git \
			tmp2/.gitignore \
			tmp2/LICENSE \
			u3836.se
		mv tmp2 u3836.se

		sleep 15s

		${pkgs.git}/bin/git clone \
			https://github.com/liamjardine/liamjardine.github.io tmp3
		rm -rf \
			tmp3/.git \
			liamjardine.se
		mv tmp3 liamjardine.se

		sleep 15s

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/secure-passwords tmp4
		rm -rf \
			tmp4/.git
			tmp4/LICENSE \
			secure-passwords
		mv tmp4 secure-passwords

		sleep 15s

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/nuschka tmp5
		rm -rf \
			tmp5/.git
			nuschka.u3836.se
		mv tmp5 nuschka.u3836.se
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
		binfmt.emulatedSystems = [];
		loader.raspberryPi.firmwareConfig = ''
			gpu_mem=320
			dtparam=audio=on
		'';
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
		dev.enable = false;
		man.generateCaches = false;
	};

	environment.systemPackages = with pkgs; [
		micro
		git
		man-pages
		man-pages-posix
		hugo

		jellyfin
		jellyfin-web
		jellyfin-ffmpeg

		update-website
		update-keys
		update-system
	];

	users.users = {
		u3836 = {
			isNormalUser = true;
			extraGroups = [ "wheel" "jellyfin" ];
		};
		nyerik = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		pingu = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		kryddan = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		koko = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		rachel-spechtachel = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
		maky.isNormalUser = true;
		liam.isNormalUser = true;
		enaya.isNormalUser = true;
		ibra.isNormalUser = true;

		jellyfin = {
			extraGroups = [ "jellyfin" "video" ];
		};
		root.openssh.authorizedKeys.keys = [
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNXgCDGyWMeQBTCloSMMEASjOLjvIOcx+HazUOrS3OR"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGzlJyY+rRehRff2s9aL8XtA6flDCqnLBz0AN7q50ivU"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJB8NV4FJc7y9gBDTBtfenUSm97Hn1eFRjmwMnILB737"
			"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOKctWKuIyHNzEe6hPt/1B/elI+0hvXKjLgUS5Kiz15o"
		];
	};

	hardware = {
		graphics.enable = true;
		pulseaudio.enable = true;
	};
	powerManagement.cpuFreqGovernor = "ondemand";
	sound.enable = true;

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
		jellyfin = {
			enable = true;
			openFirewall = true;
		};
		cron = {
			enable = true;
			systemCronJobs =
				let update-keys' = "${update-keys}/bin/update-keys";
				in [
					("* * * * * u3836 "
					 + "${pkgs.neofetch}/bin/neofetch > /tmp/eurydice-status "
					 + "&& ${pkgs.git}/bin/git -C /etc/nixos/dots rev-parse HEAD >> /tmp/eurydice-status "
					 + "&& SYSTEMD_COLORS=true systemctl status nginx | head -n3 >> /tmp/eurydice-status "
					 + "&& SYSTEMD_COLORS=true systemctl status jellyfin | head -n3 >> /tmp/eurydice-status "
					 + "&& SYSTEMD_COLORS=true systemctl status mullvad-daemon | head -n3 >> /tmp/eurydice-status "
					)
					("*/05 * * * * enaya ${update-keys'} Enayaaa")
					("*/05 * * * * u3836 ${update-keys'} SKyletoft")
					("*/05 * * * * pingu ${update-keys'} The1Penguin")
					("*/05 * * * * rachel-spechtachel ${update-keys'} rachelambda")
					("*/05 * * * * liam ${update-keys'} liamjardine")
					("*/05 * * * * kryddan ${update-keys'} ErikOrtenberg")
					("*/05 * * * * koko ${update-keys'} KokoRobinn")
					("*/05 * * * * ibra ${update-keys'} FlySlime")

					("*/05 * * * * root ${update-website}/bin/update-website")
					("*/05 * * * * root ${update-system}/bin/update-system")
				];
		};
		lorri = {
			enable = true;
			package = pkgs.lorri;
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
				"dhack.u3836.se" = {
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
					forceSSL = true;
					enableACME = true;
					locations."/".proxyPass = "http://127.0.0.1:8096";
				};
				"transmission.u3836.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/u3836.se/transmission";
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
				"nuschka.u3836.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/nuschka.u3836.se";
				};
				"static-test.u3836.se" = {
					addSSL = true;
					enableACME = true;
					root = "/var/www/test";
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
