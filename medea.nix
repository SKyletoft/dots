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
		/etc/nixos/hardware-configuration.nix
		<nixos-hardware/lenovo/thinkpad/x1/yoga/7th-gen>
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
		supportedFilesystems = [ "ntfs" ];
		binfmt.emulatedSystems = [ "aarch64-linux" ];
		kernelPackages = pkgs.linuxPackages_xanmod;
	};

	hardware = {
		opengl.enable = true;
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

		github-runners = {
			runner5 = {
				enable = true;
				name = "amba-runner6-medea";
				tokenFile = "/etc/nixos/secret/amba-github-runner-token";
				url = "https://github.com/lokegustafsson/amba";
				extraLabels = [
					"nixos"
					"performant"
					"X64"
					"impure-persistent-amba-data-dir"
				];
				extraEnvironment = {
					AMBA_DATA_DIR = "%S/github-runner/amba-runner5/amba";
				};
			};
		};

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
			enable = true;
			package = pkgs.ananicy-cpp;
		};

		earlyoom.enable = true;

		thermald.enable = true;

		# flatpak.enable = true;
		# openssh.enable = true;
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
	};

	system.stateVersion = "21.11";
}
