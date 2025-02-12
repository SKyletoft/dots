{ config, pkgs, lib, inputs, ... }:

let
	waylandSupport = true;
	windowsFonts = false;
	nativeBuild = false;

	setup-system = pkgs.callPackage ./packages/setup-system.nix {};
	update-system = pkgs.callPackage ./packages/update-system.nix {};
	update-keys = pkgs.callPackage ./packages/update-keys.nix {};
in {
	imports = [ # Include the results of the hardware scan.
		medea/hardware-configuration.nix
	];

	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
			packageOverrides = pkgs: {
				vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
			};
		};
		overlays = (import ./overlays.nix) nativeBuild;
	} // (if nativeBuild then {
		localSystem =  {
			gcc.arch = "alderlake";
			gcc.tune = "alderlake";
			system = "x86_64-linux";
		};
	} else {});

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
				"https://nix.u3836.se/"
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
		supportedFilesystems = [ "ntfs" ];
		binfmt.emulatedSystems = [ "aarch64-linux" ];
		kernelModules = [ "xpad" "hid-nintendo" "xone" "xpadneo" ];
		extraModulePackages = with config.boot.kernelPackages; [
			xone
			xpadneo
			(callPackage ./packages/xpad.nix {})
		];
	};

	hardware = {
		graphics = {
			enable = true;
			extraPackages = with pkgs; [
				# intel-compute-runtime
				# intel-ocl
				# ocl-icd
				intel-media-driver
				# vaapiIntel
				vaapiVdpau
				libvdpau-va-gl
			];
		};
		# Camera
		ipu6 = {
			enable = true;
			platform = "ipu6ep";
		};
		cpu.intel.updateMicrocode = true;
	};


	time.timeZone = "Europe/Stockholm";

	networking = {
		hostName = "medea";
		networkmanager = {
			enable = true;
			wifi.powersave = true;
			/*
			# And this imperative hack
			$ cat /etc/modprobe.d/intel_wifi.conf
			options iwlmvm power_scheme=1
			options iwlwifi power_save=Y power_level=5
			*/
		};
		firewall = {
			allowedTCPPorts
				= [ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1401 ] # Mullvad
				++ [ 25565 ];
			allowedUDPPorts
				= [ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 ] # Mullvad
				++ [ 25565 ];
		};
	};

	console.useXkbConfig = true;

	# VMM
	virtualisation = {
		libvirtd.enable = true;
		spiceUSBRedirection.enable = true;
		docker.enable = true;
	};

	powerManagement.powertop.enable = true;

	services = {
		xserver = {
			enable = true;

			displayManager.gdm = {
				enable = true;
				wayland = waylandSupport;
			};
			desktopManager.gnome.enable = true;

			xkb = {
				options = "caps:swapescape";
				extraLayouts.se-good = {
					description = "Swedish, but good";
					languages = [ "se" ];
					symbolsFile = symbols/se-good;
				};
			};
		};

		gnome = {
			gnome-keyring.enable = true;
			core-shell.enable = true;
			core-os-services.enable = true;
			core-utilities.enable = false;
			core-developer-tools.enable = false;
			games.enable = false;
		};

		ananicy = {
			enable = false;
			package = pkgs.ananicy-cpp;
		};

		earlyoom = {
			enable = true;
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

		thermald.enable = true;

		printing.enable = true;

		thinkfan = {
			enable = false;
		};

		fprintd = {
			enable = false;
			tod = {
				enable = false;
				# driver = todo;
			};
		};

		flatpak.enable = false;
		openssh = {
			enable = false;
		};
		pcscd.enable = true;
		mullvad-vpn.enable = true;

		cron = {
			enable = true;
			systemCronJobs = [
				# Every hour, check for store corruption
				# "0 * * * * root nix-store --repair --verify --check-contents || su u3836 -c ${pkgs.libnotify}/bin/notify-send 'Nix store corruption!'"
			];
		};
		ollama.enable = true;
	};

	users.users.u3836 = {
		description = "Samuel Kyletoft";
		home = "/home/u3836";
		isNormalUser = true;
		extraGroups = [
			"wheel" # Enable ‘sudo’ for the user.
			"networkmanager"
			"libvirtd"
			"dialout"
			"docker"
			"vboxusers"
			"video"
			"adbusers"
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
			git
			wally-cli
			zsa-udev-rules
			powertop
			pinentry
			# inputs.nixGL.packages.${system}.nixGLIntel

			setup-system
			update-keys
			update-system

			gnome-tweaks
		] ++ (with pkgs.gnomeExtensions; [
			brightness-control-using-ddcutil
			unite
			just-perfection
			add-username-to-top-panel
			blur-my-shell
			appindicator
			hot-edge
			caffeine
			sound-output-device-chooser
			fuzzy-app-search
			pop-shell
			burn-my-windows
			dotspaces
			hibernate-status-button
		]);
		sessionVariables = {
			MOZ_ENABLE_WAYLAND = "1";
			EDITOR = "nvim";
			MUTTER_DEBUG_FORCE_KMS_MODE = "simple";
		};
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
		# Increase max open files limit
		pam.loginLimits = [{
			domain = "*";
			type = "soft";
			item = "nofile";
			value = "32768";
		}];
	};

	fonts.packages = with pkgs; [
		cascadia-code
		fantasque-sans-mono
		roboto
		iosevka # Update to nerdfont version when I update medea
		(pkgs.callPackage ./packages/0xproto.nix {})
		(pkgs.callPackage ./packages/monaspace.nix {})
		(nerdfonts.override {fonts = [
			"FiraCode"
			"DroidSansMono"
			"RobotoMono"
		];})
	] ++ (if windowsFonts then [
		winePackages.fonts
		vistafonts
		corefonts
	] else []);

	programs = {
		sway.enable = false;
		kdeconnect = {
			enable = true;
			package = pkgs.gnomeExtensions.gsconnect;
		};
		xwayland.enable = waylandSupport;
		steam = {
			enable = true;
			gamescopeSession.enable = true;
			package = pkgs.steam.override {
				extraPkgs = p: with p; [
					xorg.libXcursor
					xorg.libXi
					xorg.libXinerama
					xorg.libXScrnSaver
					libpng
					libpulseaudio
					libvorbis
					stdenv.cc.cc.lib
					libkrb5
					keyutils
				];
			};
		};
		dconf.enable = true;
		xonsh.enable = false;
		ssh.startAgent = true;
		adb.enable = true;
		nix-ld.enable = true;
		gnupg.agent = {
			enable = true;
			pinentryPackage = pkgs.pinentry-gtk2; # GTK2 by recommendation of nixos-discourse
		};
	};

	system.stateVersion = "21.11";
}
