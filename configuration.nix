# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
	waylandSupport = false;
in {
	imports = [ # Include the results of the hardware scan.
		/etc/nixos/hardware-configuration.nix
	];

	nixpkgs.config = {
		allowUnfree = true;
		allowBroken = false;
	};

	# Use the systemd-boot EFI boot loader.
	boot = {
		loader = {
			systemd-boot.enable = true;
			efi.canTouchEfiVariables = true;
		};
		kernelPackages = pkgs.linuxPackages_zen;
		supportedFilesystems = [ "ntfs" ];
		# extraModulePackages = [ stable_pkgs.linuxPackages_zen.broadcom_sta ];
	};

	hardware = {
		opengl.enable = true;
		nvidia = {
			modesetting.enable = true;
			# package = pkgs.linuxPackages_zen.nvidia_x11_beta;
		};
	};

	# Set your time zone.
	# time.timeZone = "Europe/Amsterdam";

	# The global useDHCP flag is deprecated, therefore explicitly set to false here.
	# Per-interface useDHCP will be mandatory in the future, so this generated config
	# replicates the default behaviour.
	networking = {
		hostName = "skyletoft-ii-nix"; # Define your hostname.
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
		# firewall = {
		# 	allowedTCPPorts = [ ... ];
		# 	allowedUDPPorts = [ ... ];
		# };
		# Or disable the firewall altogether.
		# firewall.enable = false;
	};


	# Select internationalisation properties.
	# i18n.defaultLocale = "en_US.UTF-8";
	console = {
		useXkbConfig = true;
		font = "FantasqueSansMono";
	# 	keyMap = "us";
	};

	services = {
		xserver = {
			# Enable the X11 windowing system.
			enable = true;
			# Enable the GNOME Desktop Environment.
			displayManager.gdm = {
				enable = true;
				wayland = waylandSupport;
			};

			desktopManager.gnome.enable = true;

			videoDrivers = [ "nvidia" ];

			# Configure keymap in X11
			layout = "se";
			xkbVariant = "nodeadkeys";
			xkbModel = "apex300";
			# xkbOptions = "eurosign:e";

			# Enable touchpad support (enabled default in most desktopManager).
			# libinput.enable = true;
		};

		gnome.gnome-keyring.enable = true;

		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};

		# flatpak.enable = true;

		# Enable CUPS to print documents.
		# printing.enable = true;

		# Enable the OpenSSH daemon.
		# openssh.enable = true;
	};

	# Flatpak nonsense
	# xdg.portal.extraPortals = [
	# 	pkgs.xdg-desktop-portal-gtk
	# ];

	# Enable sound.
	# sound.enable = true;
	# hardware.pulseaudio.enable = true;

	users.users.u3836 = {
		description = "Samuel Kyletoft";
		home = "/home/u3836";
		isNormalUser = true;
		extraGroups = [ "wheel" "networkmanager" ]; # Enable ‘sudo’ for the user.
	};

	environment = {
		# List packages installed in system profile. To search, run:
		# $ nix search wget
		systemPackages = with pkgs; [
			nano
			git
			wget
			ffmpeg

			fd
			exa
			tealdeer
			zoxide
			bat
			ripgrep
			bottom
			du-dust
			ranger
		];
		sessionVariables = {
			EDITOR = "nvim";
		};
	};

	fonts.fonts = with pkgs; [
		cascadia-code
		fantasque-sans-mono
		(nerdfonts.override {fonts = [
			"FiraCode"
			"DroidSansMono"
			"FantasqueSansMono"
		];})
	];

	# Some programs need SUID wrappers, can be configured further or are
	# started in user sessions.
	programs = {
		sway.enable  = false;
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
		xwayland.enable = waylandSupport;
		dconf.enable = true;
	};

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "21.05"; # Did you read the comment?

}
