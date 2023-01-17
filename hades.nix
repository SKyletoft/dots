# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
	waylandSupport = false;
	windowsFonts = false;
	nativeBuild = false;
in {
	imports = [ # Include the results of the hardware scan.
		/etc/nixos/hardware-configuration.nix
	];

	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
			# cudaSupport = true;
		};
		overlays = [
			(final: prev: {
				cascadia-code-greek = prev.cascadia-code.overrideAttrs(old: {
					url = "";
				});
			})
			(self: super: {
				gnome = super.gnome.overrideScope' (gself: gsuper: {
					mutter = gsuper.mutter.overrideAttrs (oldAttrs: {
						patches = [ ./1441.patch ] ++ oldAttrs.patches;
					});
				});
			})
		] ++ (if waylandSupport then [
			(final: prev: {
				wlroots = prev.wlroots.overrideAttrs(old: {
					postPatch = "sed -i 's/assert(argb8888 &&/assert(true || argb8888 ||/g' 'render/wlr_renderer.c'";
				});
			})
		] else []) ++ (if nativeBuild then [
			(self: super: {
				stdenv = super.impureUseNativeOptimizations super.stdenv;
			})
		] else []);
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
		};
		gc = {
			automatic = true;
			dates = "weekly";
			options = "--delete-older-than 30d";
		};
		# Free up to 10 GiB whenever there is less than 1GiB left.
		extraOptions = ''
			min-free = ${toString (1024 * 1024 * 1024)}
			experimental-features = nix-command flakes
		'';
	};

	# Use the systemd-boot EFI boot loader.
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
			config.boot.kernelPackages.ddcci-driver
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

	# Set your time zone.
	time.timeZone = "Europe/Stockholm";

	# The global useDHCP flag is deprecated, therefore explicitly set to false here.
	# Per-interface useDHCP will be mandatory in the future, so this generated config
	# replicates the default behaviour.
	networking = {
		hostName = "hades"; # Define your hostname.
		# wireless.enable = true;  # Enables wireless support via wpa_supplicant.
		# useDHCP = false;
		networkmanager.enable = true;
		interfaces = {
			enp0s31f6.useDHCP = false; # WiFi card
			# wlp0s20f0u1.useDHCP = false; # USB WiFi dongle
		};
		# Configure network proxy if necessary
		# proxy = {
		# 	default = "http://user:password@proxy:port/";
		# 	noProxy = "127.0.0.1,localhost,internal.domain";
		# };

		# Open ports in the firewall.
		firewall = {
			allowedTCPPorts =
				[ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1401 ]; # Mullvad
			allowedUDPPorts =
				[ 80 443 6530 8000 8080 12825 ] # Development
				++ [ 53 1194 1195 1196 1197 1399 1391 1392 1393 1400 ]; # Mullvad
			# Or disable the firewall altogether.
			# enable = false;
		};
	};


	# Select internationalisation properties.
	# i18n.defaultLocale = "en_US.UTF-8";
	console = {
		useXkbConfig = true;
		font = "FantasqueSansMono";
		# keyMap = "us";
	};

	# VMM
	virtualisation = {
		libvirtd.enable = true;
		spiceUSBRedirection.enable = true;
		# docker.enable = true;
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

			# Enable touchpad support (enabled default in most desktopManager).
			# libinput.enable = true;
		};

		pipewire = {
			media-session.enable = true;
			wireplumber.enable = false;
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

		mullvad-vpn.enable = true;

		fwupd.enable = true;

		# flatpak.enable = true;

		# Enable CUPS to print documents.
		# printing.enable = true;

		# Enable the OpenSSH daemon.
		openssh = {
			enable = true;
			passwordAuthentication = false;
		};
	};

	# Flatpak nonsense
	# xdg.portal.extraPortals = [
	# 	pkgs.xdg-desktop-portal-gtk
	# ];

	# Enable sound.
	# sound.enable = true;
	# hardware.pulseaudio.enable = true;
	# systemd.user.services.pipewire-pulse.path = [ pkgs.pulseaudio ];

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
		man.generateCaches = true;
	};

	environment = {
		systemPackages = with pkgs; [
			nano
			ffmpeg
			man-pages
			man-pages-posix
			ddcutil
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

	# Some programs need SUID wrappers, can be configured further or are
	# started in user sessions.
	programs = {
		sway.enable = waylandSupport;
		xwayland.enable = waylandSupport;

		steam.enable = true;
		kdeconnect = {
			enable = true;
			package = pkgs.gnomeExtensions.gsconnect;	
		};
		# mtr.enable = true;
		# gnupg.agent = {
		# 	enable = true;
		# 	enableSSHSupport = true;
		# };
		dconf.enable = true;
		adb.enable = true;
	};

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "21.05"; # Did you read the comment?

}
