# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
	waylandSupport = true;
	windowsFonts = false;
	nativeBuild = false;
in {
	imports = [ # Include the results of the hardware scan.
		medea/hardware-configuration.nix
	];

	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
			packageOverrides = pkgs: {
				vaapiIntel = pkgs.vaapiIntel.override {
					enableHybridCodec = true;
				};
			};
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
				"nix.u3836.se:t7H/bFWi14aBFYPE5A00eEQawd7Ssl/fXbq/2C+Bsrs="
				"nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
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
		opengl = {
			enable = true;
			extraPackages = with pkgs; [
				intel-compute-runtime
				intel-ocl
				ocl-icd
			# 	intel-media-driver
			# 	vaapiIntel
			# 	vaapiVdpau
			# 	libvdpau-va-gl
			];
		};
		cpu.intel.updateMicrocode = true;
	};


	time.timeZone = "Europe/Stockholm";

	networking = {
		hostName = "medea";
		networkmanager.enable = true;
		firewall = {
			allowedTCPPorts = [ 80 6530 8080 12345 ];
			allowedUDPPorts = [ 80 6530 8080 12345 ];
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

			xkbOptions = "caps:swapescape";
			extraLayouts.se-good = {
				description = "Swedish, but good";
				languages = [ "se" ];
				symbolsFile = symbols/se-good;
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

		earlyoom.enable = true;

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
		openssh.enable = false;
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
		];
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

	fonts.packages = with pkgs; [
		cascadia-code
		fantasque-sans-mono
		roboto
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
		steam.enable = true;
		dconf.enable = true;
		xonsh.enable = false;
		ssh.startAgent = true;
		adb.enable = true;
	};

	system.stateVersion = "21.11";
}
