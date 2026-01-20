{ config, pkgs, lib, nixGL, waylandSupport, windowsFonts, nativeBuild, nativeArch, flatpak, ... }:

let
	setup-system = pkgs.callPackage ./packages/setup-system.nix {};
	update-system = pkgs.callPackage ./packages/update-system.nix {};
	update-keys = pkgs.callPackage ./packages/update-keys.nix {};
in {
	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
		};
		overlays = (import ./overlays.nix) nativeBuild nativeArch;
	};

	hardware.keyboard.zsa.enable = true;

	nix = {
		settings = {
			auto-optimise-store = true;
			system-features = [
				"benchmark"
				"big-parallel"
				"gccarch-alderlake"
				"kvm"
				"nixos-test"
			];
			# post-build-hook = "/home/u3836/dots/upload-to-cache.sh";
			substituters = [
				"https://nix-community.cachix.org"
				"https://cache.nixos.org/"
				# "https://nix.u3836.se/"
				"https://cache.iog.io"
			];
			trusted-public-keys = [
				"nix.u3836.se:t7H/bFWi14aBFYPE5A00eEQawd7Ssl/fXbq/2C+Bsrs="
				"nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
				"hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
			];
		};
		gc = {
			automatic = true;
			dates = "weekly";
			options = "--delete-older-than 30d";
		};
		# Free up to 16 GiB whenever there is less than 1GiB left.
		extraOptions = ''
			min-free = ${toString (16 * 1024 * 1024 * 1024)}
			experimental-features = nix-command flakes
		'';
	};

	# Use the systemd-boot EFI boot loader.
	boot = {
		loader = {
			systemd-boot.enable = true;
			efi.canTouchEfiVariables = true;
		};
		kernelPackages = pkgs.linuxPackages_xanmod;
		supportedFilesystems = {
			exfat = true;
			ntfs = true;
			sshfs = true;
		};
		binfmt.emulatedSystems = [ "aarch64-linux" ];
		kernelModules = [ "xpad" "hid-nintendo" "xone" "xpadneo" ];
		# extraModulePackages = with config.boot.kernelPackages; [
		#	xone
		#	xpadneo
		#	(callPackage ./packages/xpad.nix {})
		# ];
	};

	time.timeZone = "Europe/Stockholm";

	networking = {
		networkmanager.enable = true;
		firewall = {
			allowedTCPPorts
				= [ 80 443 6530 8000 8080 8081 12825 ] # Development
				++ [ 53 1401 ] # Mullvad
				++ [ 25565 ];
			allowedUDPPorts
				= [ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 ] # Mullvad
				++ [ 25565 ];

			# Virtualisation networking
			trustedInterfaces = [ "virbr0" ];
			# backend = "iptables";
		};
	};

	console.useXkbConfig = true;

	# VMM
	virtualisation = {
		libvirtd = {
			enable = true;
			# hooks.network = ''
			#	virsh net-autostart default
			# '';
			# extraConfig = ''
			#	firewall_backend = "iptables";
			# '';
		};
		spiceUSBRedirection.enable = true;
		docker.enable = true;
	};

	services = {
		xserver = {
			enable = true;

			xkb = {
				options = "caps:swapescape";
				extraLayouts.se-good = {
					description = "Swedish, but good";
					languages = [ "se" ];
					symbolsFile = symbols/se-good;
				};
			};
		};

		displayManager.gdm = {
			enable = true;
			wayland = waylandSupport;
		};
		desktopManager.gnome.enable = true;

		gnome = {
			gnome-keyring.enable = true;
			core-shell.enable = true;
			core-os-services.enable = true;
			core-apps.enable = false;
			core-developer-tools.enable = false;
			games.enable = false;
		};

		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
			extraRules = [
				{
					"name" = "gamescope";
					"nice" = -20;
				}
			];
		};

		earlyoom = {
			enable = false;
			extraArgs = [
				"--prefer '(^|/)(gcc|g\+\+|cc1plus|ghc|rustc|a\.out)$'"
				"--avoid  '(^|/)(firefox-develop|firefox-bin)$'"
				"--ignore '(^|/)(init|mutter|gnome-shell|Xorg)$'"
			];
			enableNotifications = true;
		};

		udev.extraRules = ''
			ACTION=="add", \
				ATTRS{idVendor}=="2dc8", \
				ATTRS{idProduct}=="3106", \
				RUN+="${pkgs.kmod}/bin/modprobe xpad", \
				RUN+="${pkgs.bash}/bin/sh -c 'echo 2dc8 3106 > /sys/bus/usb/drivers/xpad/new_id'"
		'';

		mullvad-vpn.enable = true;

		flatpak.enable = flatpak;

		ddccontrol.enable = true;

		journald.extraConfig = "SystemMaxUse=256M";

		fwupd.enable = true;

		gpm.enable = true;

		openssh = {
			enable = true;
			settings = {
				PasswordAuthentication = false;
				X11Forwarding = true;
			};
		};

		cron = {
			enable = true;
			systemCronJobs = [
				# Every hour, check for store corruption
				# "0 * * * * root nix-store --repair --verify --check-contents || su u3836 -c ${pkgs.libnotify}/bin/notify-send 'Nix store corruption!'"
			];
		};

		ollama = {
			enable = true;
			openFirewall = true;
		};
	};

	# Flatpak nonsense
	xdg.portal.extraPortals = if flatpak then [ pkgs.xdg-desktop-portal-gtk ] else [];

	users.users.u3836 = {
		description = "Samuel Kyletoft";
		home = "/home/u3836";
		isNormalUser = true;
		extraGroups = [
			"wheel"           # sudo
			"networkmanager"  # networking
			"libvirtd"        # virtual machines
			"dialout"         # md407
			"docker"          # docker
			"i2c"             # screen brightness
			"adbusers"        # android debugging (screen sharing)
			"plugdev"         # keyboard flashing
			"video" "render"  # blender
			"vboxusers"
			"camera"
		];
		shell = pkgs.bash;
	};

	documentation = {
		dev.enable = true;
		doc.enable = true;
		info.enable = true;
		nixos.includeAllModules = true;
		man = {
			enable = true;
			generateCaches = true;
		};
	};

	environment = {
		systemPackages = with pkgs; [
			nano
			ffmpeg
			man-pages
			man-pages-posix
			cachix
			wally-cli
			zsa-udev-rules
			powertop
			pinentry-gnome3
			android-tools # adb

			nix-output-monitor

			nixGL.packages.${system}.nixGLIntel

			xorg.xkill
			xwayland-satellite
			logiops

			steamcmd

			setup-system
			update-keys
			update-system

			gnome-tweaks
		] ++ (with pkgs.gnomeExtensions; [
			add-username-to-top-panel
			appindicator
			blur-my-shell
			brightness-control-using-ddcutil
			caffeine
			hibernate-status-button
			hot-edge
			just-perfection
			pop-shell
			unite
		]);

		sessionVariables = {
			EDITOR = "nvim";
			MUTTER_DEBUG_FORCE_KMS_MODE = "simple";
		} // (if waylandSupport then {
			MOZ_ENABLE_WAYLAND = "1";
			NIXOS_OZONE_WL = "1";
			ELECTRON_OZONE_PLATFORM_HINT = "auto";
		} else {});
	};

	security = {
		# Replace sudo with doas
		sudo.enable = false;
		doas = {
			enable = true;
			extraRules = [{
				groups = [ "wheel" ];
				keepEnv = true;
				persist = true;
			}];
		};
		pam = {
			# Increase max open files limit
			loginLimits = [{
				domain = "*";
				type = "soft";
				item = "nofile";
				value = "32768";
			}];
			services.greetd.enableGnomeKeyring = true;
		};
	};

	fonts.packages = with pkgs.nerd-fonts; [
		fantasque-sans-mono
		iosevka
		_0xproto
		monaspace
		fira-code
		droid-sans-mono
		roboto-mono
	] ++ (with pkgs; [
		roboto
		cascadia-code
		cantarell-fonts
		material-symbols
	]) ++ (if windowsFonts then [
		winePackages.fonts
		vistafonts
		corefonts
	] else []);

	programs = {
		git = {
			enable = true;
			lfs.enable = true;
		};
		sway.enable = false;
		bazecor.enable = true;
		kdeconnect = {
			enable = true;
			package = pkgs.gnomeExtensions.gsconnect;
		};
		niri.enable = waylandSupport;
		hyprland.enable = waylandSupport;
		xwayland.enable = false;
		gamemode.enable = true;
		gamescope = {
			enable = true;
			capSysNice = false;
		};
		steam = {
			enable = true;
			remotePlay.openFirewall = true;
			package = pkgs.steam.override {
				extraPkgs = p: with p; [
					xorg.libXcursor
					xorg.libXi
					xorg.libXinerama
					xorg.libXScrnSaver
					libpng
					libpulseaudio
					libvorbis
					libkrb5
					libdecor
					stdenv.cc.cc.lib
					keyutils
				];
			};
		};
		dconf.enable = true;
		xonsh.enable = false;
		nix-ld.enable = true;
		gnupg.agent = {
			enable = true;
			pinentryPackage = pkgs.pinentry-gnome3;
		};
	};
}
