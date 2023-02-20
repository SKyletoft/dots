{ config, pkgs, ... }:
let
	waylandSupport = false;
	windowsFonts = false;
	nativeBuild = false;
	flatpak = false;
in {
	imports = [
		/etc/nixos/hardware-configuration.nix
		/etc/nixos/cachix.nix
	];

	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
		};
		overlays = (import ./overlays.nix) nativeBuild;
	} // (if nativeBuild then {
		localSystem =  {
			gcc.arch = "skylake";
			gcc.tune = "skylake";
			system = "x86_64-linux";
		};
	} else {});

	nix = {
		settings = {
			auto-optimise-store = true;
			system-features = [
				"benchmark"
				"big-parallel"
				"gccarch-skylake"
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
		kernelPackages = pkgs.linuxPackages_zen;
		supportedFilesystems = [ "ntfs" ];
		binfmt.emulatedSystems = [ "aarch64-linux" ];
		kernelModules = [ "i2c-dev" "xpad" "hid-nintendo" "xone" "xpadneo" ];
		extraModulePackages = [
			config.boot.kernelPackages.xone
			config.boot.kernelPackages.xpadneo
			(config.boot.kernelPackages.callPackage ./packages/xpad.nix {})
		];
	};

	hardware = {
		opengl.enable = true;
		nvidia.modesetting.enable = waylandSupport;
		cpu.intel.updateMicrocode = true;
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
		hostName = "hades";
		networkmanager.enable = true;
		interfaces.enp0s31f6.useDHCP = false;
		firewall = {
			allowedTCPPorts
				= [ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1401 ]; # Mullvad
			allowedUDPPorts
				= [ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 ]; # Mullvad
		};
	};

	console.useXkbConfig = true;

	virtualisation = {
		libvirtd.enable = true;
		spiceUSBRedirection.enable = true;
		docker.enable = false;
	};

	services = {
		xserver = {
			enable = true;
			displayManager.gdm = {
				enable = true;
				wayland = waylandSupport;
			};

			desktopManager.gnome.enable = true;

			videoDrivers = [ "nvidia" ];

			xkbOptions = "caps:swapescape";
			extraLayouts.se-good = {
				description = "Swedish, but good";
				languages = [ "se" ];
				symbolsFile = symbols/se-good;
			};

			screenSection = ''
				Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
				Option         "metamodes" "nvidia-auto-select +1440+0 {ForceFullCompositionPipeline=On}"
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

		earlyoom.enable = true;

		# https://discourse.nixos.org/t/how-to-enable-ddc-brightness-control-i2c-permissions/20800/2
		udev.extraRules = ''
			KERNEL=="i2c-[0-9]*", \
				GROUP="i2c", \
				MODE="0660", \
				SUBSYSTEM=="i2c-dev", \
				ACTION=="add", \
				ATTR{name}=="NVIDIA i2c adapter*", \
				TAG+="ddcci", \
				TAG+="systemd", \
				ENV{SYSTEMD_WANTS}+="ddcci@$kernel.service"
			ACTION=="add", \
				ATTRS{idVendor}=="2dc8", \
				ATTRS{idProduct}=="3106", \
				RUN+="${pkgs.kmod}/bin/modprobe xpad", \
				RUN+="${pkgs.bash}/bin/sh -c 'echo 2dc8 3106 > /sys/bus/usb/drivers/xpad/new_id'"
		'';

		journald.extraConfig = "SystemMaxUse=256M";
		# mullvad-vpn.enable = true;
		fwupd.enable = true;
		flatpak.enable = flatpak;
		openssh = {
			enable = true;
			settings.PasswordAuthentication = false;
		};
	};

	# Flatpak nonsense
	xdg.portal.extraPortals = if flatpak then [ pkgs.xdg-desktop-portal-gtk ] else [];

	users.groups.i2c = {};
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
		];
		sessionVariables = {
			MUTTER_DEBUG_FORCE_KMS_MODE = "simple";
			WEBKIT_DISABLE_COMPOSITING_MODE = "1";
			__GL_SHADER_DISK_CACHE_SKIP_CLEANUP = "1";
		} // (if waylandSupport then {
			LIBVA_DRIVER_NAME = "nvidia";
			CLUTTER_BACKEND = "wayland";
			XDG_SESSION_TYPE = "wayland";
			QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
			MOZ_ENABLE_WAYLAND = "1";
			GBM_BACKEND = "nvidia-drm";
			__GLX_VENDOR_LIBRARY_NAME = "nvidia";
			WLR_NO_HARDWARE_CURSORS = "1";
			WLR_BACKEND = "vulkan";
			QT_QPA_PLATFORM = "wayland";
			GDK_BACKEND = "wayland";
		} else {});
	};

	security = {
		# Replace sudo with doas
		sudo.enable = false;
		doas = {
			enable = true;
			extraRules = [{
				users = [ "u3836" ];
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

	fonts.fonts = with pkgs; [
		cascadia-code
		fantasque-sans-mono
		roboto
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

		steam.enable = true;
		kdeconnect = {
			enable = true;
			package = pkgs.gnomeExtensions.gsconnect;	
		};
		dconf.enable = true;
		adb.enable = true;
	};

	system.stateVersion = "21.05";
}
