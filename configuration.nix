# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
	waylandSupport = false;
	compiledKeyboardLayout = pkgs.runCommand "keyboard-layout" {} "${pkgs.xorg.xkbcomp}/bin/xkbcomp ${./layout.xkb} $out";
in {
	imports = [ # Include the results of the hardware scan.
		/etc/nixos/hardware-configuration.nix
		./desktop.nix
	];

	nixpkgs.config = {
		allowUnfree = true;
		allowBroken = false;
	};

	nix = {
		settings.auto-optimise-store = true;
		gc = {
			automatic = true;
			dates = "weekly";
			options = "--delete-older-than 30d";
		};
		# Free up to 10 GiB whenever there is less than 1GiB left.
		extraOptions = ''
			min-free = ${toString (1024 * 1024 * 1024)}
			max-free = ${toString (10 * 1024 * 1024 * 1024)}
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
	};

	# Select internationalisation properties.
	# i18n.defaultLocale = "en_US.UTF-8";
	console = {
		useXkbConfig = true;
		font = "FantasqueSansMono";
	# 	keyMap = "us";
	};

	# VMM
	virtualisation = {
		libvirtd.enable = true;
		spiceUSBRedirection.enable = true;
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

			# Backup layout
			layout = "se";
			xkbVariant = "nodeadkeys";
			xkbModel = "apex300";

			# Normal layout
			displayManager.sessionCommands = "${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledKeyboardLayout} $DISPLAY";

			# Enable touchpad support (enabled default in most desktopManager).
			# libinput.enable = true;
		};

		gnome.gnome-keyring.enable = true;

		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};

		earlyoom.enable = true;
	};

	# Enable sound.
	# sound.enable = true;
	# hardware.pulseaudio.enable = true;

	users.users.u3836 = {
		description = "Samuel Kyletoft";
		home = "/home/u3836";
		isNormalUser = true;
		extraGroups = [ "wheel" "networkmanager" "libvirtd" "dialout" ]; # Enable ‘sudo’ for the user.
	};

	environment = {
		# List packages installed in system profile. To search, run:
		# $ nix search wget
		systemPackages = with pkgs; [
			nano
			ffmpeg
			gnome3.adwaita-icon-theme
		];
	};

	# Replace sudo with doas
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
