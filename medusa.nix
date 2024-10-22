{ config, pkgs, ... }:

let
	waylandSupport = false;
	windowsFonts = false;
	nativeBuild = false;
	flatpak = false;

	setup-system = pkgs.callPackage ./packages/setup-system.nix {};
	update-system = pkgs.callPackage ./packages/update-system.nix {};
	update-keys = pkgs.callPackage ./packages/update-keys.nix {};
in {
	imports = [ medusa/hardware-configuration.nix ];

	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
		};
		overlays = (import ./overlays.nix) nativeBuild;
	} // (if nativeBuild then {
		localSystem =  {
			gcc.arch = "znver4";
			gcc.tune = "znver4";
			system = "x86_64-linux";
		};
	} else {});

	nix = {
		settings = {
			auto-optimise-store = true;
			system-features = [
				"benchmark"
				"big-parallel"
				"gccarch-znver4"
				"kvm"
				"nixos-test"
			];
			# post-build-hook = "/home/u3836/dots/upload-to-cache.sh";
			substituters = [
				"https://nix-community.cachix.org"
				"https://cache.nixos.org/"
				"https://nix.u3836.se/"
			];
			trusted-public-keys = [
				"nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
				"nix.u3836.se:t7H/bFWi14aBFYPE5A00eEQawd7Ssl/fXbq/2C+Bsrs="
			];
			trusted-users = [ "@wheel" ];
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
		loader = {
			systemd-boot.enable = true;
			efi.canTouchEfiVariables = true;
		};
		kernelPackages = pkgs.linuxPackages_xanmod_latest;
		supportedFilesystems = [ "ntfs" ];
		binfmt.emulatedSystems = [ "aarch64-linux" ];
		kernelModules = [ "amdgpu" "kvm-amd" "i2c-dev" "xpad" "hid-nintendo" "xone" "xpadneo" ];
		extraModulePackages = [
			config.boot.kernelPackages.xone
			config.boot.kernelPackages.xpadneo
			(config.boot.kernelPackages.callPackage ./packages/xpad.nix {})
		];
	};

	hardware = {
		graphics = {
			enable = true;
			extraPackages = with pkgs; [
				rocm-opencl-icd
				rocm-opencl-runtime
			];
		};
		cpu.amd.updateMicrocode = true;
		keyboard.zsa.enable = true;
		i2c.enable = true;
	};

	fileSystems."/mnt/SDD" = {
		device = "/dev/disk/by-label/SDD"; # Actual device is randomised for some reason
		fsType = "ntfs";
		options = [
			"allow_other"
			"x-systemd.automount"
		];
	};

	time.timeZone = "Europe/Stockholm";

	networking = {
		hostName = "medusa";
		networkmanager.enable = true;
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

	virtualisation = {
		libvirtd.enable = true;
		spiceUSBRedirection.enable = true;
		docker.enable = true;
	};

	systemd.tmpfiles.rules = [
		"L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
	];

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

			videoDrivers = [ "amdgpu" ];

			deviceSection = ''
				Option         "TearFree" "true"
				Option         "VariableRefresh" "true"
				Option         "AsyncFlipSecondaries" "true"
			'';

			screenSection = ''
				Option         "AllowIndirectGLXProtocol" "off"
				Option         "TripleBuffer" "on"
			'';
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
			enable = true;
			package = pkgs.ananicy-cpp;
		};

		earlyoom = {
			enable = true;
			extraArgs = [
				"--prefer \"gcc|ghc|rustc|a\.out\""
				"--avoid  \"mutter|gnome-shell|x|firefox\""
			];
			killHook = pkgs.writeShellScript "earlyoom-kill-hook" ''
				${pkgs.libnotify}/bin/notify-send
					'Killed $EARLYOOM_NAME ($EARLYOOM_PID) to reclaim memory'
			'';
		};

		udev.extraRules = ''
			ACTION=="add", \
				ATTRS{idVendor}=="2dc8", \
				ATTRS{idProduct}=="3106", \
				RUN+="${pkgs.kmod}/bin/modprobe xpad", \
				RUN+="${pkgs.bash}/bin/sh -c 'echo 2dc8 3106 > /sys/bus/usb/drivers/xpad/new_id'"
		'';

		journald.extraConfig = "SystemMaxUse=256M";
		mullvad-vpn.enable = true;
		fwupd.enable = true;
		flatpak.enable = flatpak;
		openssh = {
			enable = true;
			settings.PasswordAuthentication = false;
		};

		cron = {
			enable = true;
			systemCronJobs = [
				# Every hour, check for store corruption
				"0 * * * * root nix-store --repair --verify --check-contents"
			];
		};
		ollama = {
			enable = true;
			acceleration = "rocm";
		};
	};

	# Flatpak nonsense
	xdg.portal.extraPortals = if flatpak then [ pkgs.xdg-desktop-portal-gtk ] else [];

	users.groups.i2c = {};
	users.groups.plugdev = {};
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
			xorg.xkill

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
		emacs-all-the-icons-fonts
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
		sway.enable = waylandSupport;
		xwayland.enable = waylandSupport;

		gamemode.enable = true;
		steam.enable = true;
		kdeconnect = {
			enable = true;
			package = pkgs.gnomeExtensions.gsconnect;
		};
		dconf.enable = true;
		adb.enable = true;
		ssh.startAgent = true;
		nix-ld.enable = true;
	};

	system.stateVersion = "22.11";
}
