{ config, pkgs, lib, ... }:

let
	update-keys = pkgs.writeShellScriptBin "update-keys" ''
		mkdir -p ~/.ssh
		cd ~/.ssh

		${pkgs.curl}/bin/curl \
			https://github.com/$(echo $1).keys \
			> authorized_keys
		chmod 700 ~/.ssh
		chmod 644 ~/.ssh/authorized_keys
	'';
in {
	imports = [];

	nixpkgs = {
		config.allowUnfree = true;
	};

	boot = {
		supportedFilesystems = [ "exfat" ];
		loader.raspberryPi.firmwareConfig = ''
			gpu_mem=192
			dtparam=audio=on
		'';
	};

	networking.hostName = "nixos";

	fileSystems = {
		"/" = {
			device = "/dev/disk/by-label/NIXOS_SD";
			fsType = "ext4";
			options = [ "noatime" ];
		};
	};

	environment.systemPackages = with pkgs; [
		micro
		git
		update-keys
	];

	users.users = {
		u3836 = {
			isNormalUser = true;
			extraGroups = [ "wheel" ];
		};
	};

	services = {
		xserver.enable = false;
		openssh = {
			enable = true;
			settings.PasswordAuthentication = false;
		};
		cron = {
			enable = true;
			systemCronJobs = [
				("*/05 * * * * u3836 ${update-keys}/bin/update-keys SKyletoft")
			];
		};
	};

	programs = {
		ssh.startAgent = true;
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
	};

	system.stateVersion = "21.11";
}
