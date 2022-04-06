{ config, pkgs, lib, ... }:

{
	imports = [
		<nixos-hardware/raspberry-pi/4>
	];

	nixpkgs.config.allowUnfree = true;
	nix = {
		settings.auto-optimise-store = true;
		gc = {
			automatic = true;
			dates = "weekly";
			options = "--delete-older-than 30d";
		};
		extraOptions = ''
			min-free = ${toString (100 * 1024 * 1024)}
			max-free = ${toString (1024 * 1024 * 1024)}
		'';
	};

	networking = {
		hostName = "eurydice";
		interfaces.eth0.ipv4.addresses = [ {
			address = "192.168.0.200";
			prefixLength = 24;
		} ];
		defaultGateway = "192.168.0.1";
		nameservers = [ "8.8.8.8" ];
	};

	fileSystems."/" = {
		device = "/dev/disk/by-label/NIXOS_SD";
		fsType = "ext4";
		options = [ "noatime" ];
	};

	environment.systemPackages = with pkgs; [ neovim ];

	users.users.u3836 = {
		isNormalUser = true;
		extraGroups = [ "wheel" ];
		shell = pkgs.xonsh;
	};

	hardware = {
		pulseaudio.enable = false;
		# Enable GPU acceleration
		raspberry-pi."4".fkms-3d.enable = false;
	};
	powerManagement.cpuFreqGovernor = "ondemand";

	services = {
		xserver.enable = false;
		openssh = {
			enable = true;
			passwordAuthentication = false;
		};
		earlyoom.enable = true;
		ananicy = {
			enable = true;
			package = pkgs.ananicy-cpp;
		};
		nginx = {
			enable = true;
			virtualHosts = {
				"samuel.kyletoft.se" = {
					forceSSL = true;
					enableACME = true;
					locations."/".root = "/var/www/samuel.kyletoft.se";
					serverAliases = [ "*.kyletoft.se" ];
				};
			};
		};
	};

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
		acme = {
			acceptTerms = true;
			email = "samuel+acme@kyletoft.se";
		};
	};
}
