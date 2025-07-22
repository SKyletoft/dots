{ config, pkgs, lib, ... }:

let
	update-keys = pkgs.callPackage ./packages/update-keys.nix {};
	update-motd = pkgs.writeShellScriptBin "update-motd" ''
		rm /tmp/eurydice-status || true
		${pkgs.neofetch}/bin/neofetch > /tmp/eurydice-status
		${pkgs.git}/bin/git -C /etc/nixos/dots log -1 >> /tmp/eurydice-status
		echo >> /tmp/eurydice-status
		SYSTEMD_COLORS=true systemctl status nginx | head -n3 >> /tmp/eurydice-status
		SYSTEMD_COLORS=true systemctl status jellyfin | head -n3 >> /tmp/eurydice-status
		SYSTEMD_COLORS=true systemctl status mullvad-daemon | head -n3 >> /tmp/eurydice-status
		SYSTEMD_COLORS=true systemctl status github-runner-runner1 | head -n3 >> /tmp/eurydice-status
	'';
	update-status = pkgs.writeShellScriptBin "update-status" ''
		${update-motd}/bin/update-motd
		rm /var/www/status || true
		mkdir -p /var/www/status
		cat /tmp/eurydice-status | ${pkgs.ansi2html}/bin/ansi2html > /var/www/status/index.html
	'';
	update-website = pkgs.writeShellScriptBin "update-website" ''
		cd /var/www

		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/samuel.kyletoft.se tmp1
		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/valkompass_2022 tmp1-1
		${pkgs.git}/bin/git clone \
			https://github.com/SKyletoft/cv tmp0
		cd tmp0
		${pkgs.tectonic}/bin/tectonic en.tex
		${pkgs.tectonic}/bin/tectonic sv.tex
		mv en.pdf ../tmp1/cv/en/CV.pdf
		mv sv.pdf ../tmp1/cv/sv/CV.pdf
		cd ..
		rm -rf \
			tmp0 \
			tmp1/.git \
			tmp1/.gitignore \
			tmp1/LICENSE \
			tmp1-1/.git \
			tmp1-1/.gitignore \
			tmp1-1/LICENSE \
			samuel.kyletoft.se
		mv tmp1-1 tmp1/valkompass
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
			tmp5/.git \
			nuschka.u3836.se
		mv tmp5 nuschka.u3836.se

		rm -rf tmp*
	'';
in {
	boot = {
		supportedFilesystems.sshfs = true;
		binfmt.emulatedSystems = [ "x86_64-linux" ];
	};

	networking.hostName = "eurydice";

	fileSystems = {
		"/mnt/hekate" = {
			device = "u3836@192.168.0.203:/";
			fsType = "sshfs";
			options = [
				"identityfile=/home/u3836/.ssh/id_ed25519"
				"idmap=user"
				"x-systemd.automount"
				"allow_other"
				"user"
			];
		};
	};

	documentation = {
		dev.enable = false;
		man.generateCaches = false;
	};

	environment.systemPackages = with pkgs; [
		hugo
		update-motd
		update-website
		update-status
	];

	users.users = {
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
	};

	hardware.graphics.enable = true;
	services.pulseaudio.enable = true;

	services = {
		jellyfin = {
			enable = true;
			openFirewall = true;
		};
		cron = {
			enable = true;
			systemCronJobs =
				let update-keys' = "${update-keys}/bin/update-keys";
				in [
					("*/05 * * * * enaya ${update-keys'} Enayaaa")
					("*/05 * * * * pingu ${update-keys'} The1Penguin")
					("*/05 * * * * rachel-spechtachel ${update-keys'} rachelambda")
					("*/05 * * * * liam ${update-keys'} liamjardine")
					("*/05 * * * * kryddan ${update-keys'} ErikOrtenberg")
					("*/05 * * * * koko ${update-keys'} KokoRobinn")
					("*/05 * * * * ibra ${update-keys'} FlySlime")

					("*/05 * * * * root ${update-website}/bin/update-website")
					("* * * * *    u3836 ${update-status}/bin/update-status")
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
				"hekate.u3836.se" = {
					forceSSL = true;
					enableACME = true;
					root = "/var/www/u3836.se/hekate";
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
				"status.u3836.se" = {
					addSSL = true;
					enableACME = true;
					root = "/var/www/status";
				};
			};
		};
		github-runners = {
			runner1 = {
				enable = true;
				name = "runner1";
				tokenFile = "/etc/nixos/secret/thesis-runner-token";
				url = "https://github.com/SKyletoft/masters-thesis";
			};
		};
	};

	security.acme = {
		acceptTerms = true;
		defaults.email = "samuel+acme@kyletoft.se";
	};

	system.stateVersion = "21.11";
}
