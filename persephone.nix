# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
	waylandSupport = true;
in {
	imports = [ # Include the results of the hardware scan.
		/etc/nixos/hardware-configuration.nix
		<nixos-hardware/microsoft/surface>
	];

	nixpkgs = {
		config = {
			allowUnfree = true;
			allowBroken = false;
		};
		overlays = [
			(final: prev: {
				wlroots = prev.wlroots.overrideAttrs(old: {
					postPatch = "sed -i 's/assert(argb8888 &&/assert(true || argb8888 ||/g' 'render/wlr_renderer.c'";
				});
			})
			(self: super: {
				gnome = super.gnome.overrideScope' (gself: gsuper: {
					gnome-shell = gsuper.gnome-shell.overrideAttrs (oldAttrs: {
						patches = [ ./1915.patch ] ++ oldAttrs.patches;
					});
					mutter = gsuper.mutter.overrideAttrs (oldAttrs: {
						patches = [ ./1441.patch ] ++ oldAttrs.patches;
					});
				});
			})
		];
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
	};

	hardware = {
		opengl.enable = true;
	};


	# Set your time zone.
	time.timeZone = "Europe/Stockholm";

	# The global useDHCP flag is deprecated, therefore explicitly set to false here.
	# Per-interface useDHCP will be mandatory in the future, so this generated config
	# replicates the default behaviour.
	networking = {
		hostName = "persephone"; # Define your hostname.
		# wireless.enable = true;  # Enables wireless support via wpa_supplicant.
		# useDHCP = false;
		networkmanager.enable = true;
		interfaces = {
			# enp0s31f6.useDHCP = false; # WiFi card
			wlp1s0.useDHCP = true; # USB WiFi dongle
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

	console.useXkbConfig = true;

	# VMM
	virtualisation = {
		libvirtd.enable = true;
		spiceUSBRedirection.enable = true;
		docker.enable = true;
	};

	services = {
		xserver = {
			enable = true;

			displayManager.gdm = {
				enable = true;
				wayland = waylandSupport;
			};
			desktopManager.gnome.enable = true;

			extraLayouts.se-good = {
				description = "Swedish, but good";
				languages = [ "se" ];
				symbolsFile = symbols/se-good;
			};
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
		man.generateCaches = true;
	};

	environment.systemPackages = with pkgs; [
		nano
		ffmpeg
		man-pages
		man-pages-posix
	];

	environment.sessionVariables = {
		MOZ_ENABLE_WAYLAND = "1";
		EDITOR = "nvim";
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

	programs = {
		sway.enable = false;
		kdeconnect = {
			enable = true;
			package = pkgs.gnomeExtensions.gsconnect;	
		};
		xwayland.enable = waylandSupport;
		steam.enable = true;
		dconf.enable = true;
		xonsh.enable = true;
	};

	system.stateVersion = "21.11";
}
