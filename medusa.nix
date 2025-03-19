{ config, pkgs, waylandSupport, windowsFonts, nativeBuild, flatpak, ... }:

{
	nixpkgs = {
		config.rocmSupport = true;
	} // (if nativeBuild then {
		localSystem =  {
			gcc.arch = "znver4";
			gcc.tune = "znver4";
			system = "x86_64-linux";
		};
	} else {});

	boot.kernelModules = [ "amdgpu" "kvm-amd" "i2c-dev" ];

	hardware = {
		graphics.enable = true;
		cpu.amd.updateMicrocode = true;
		keyboard.zsa.enable = true;
		i2c.enable = true;
	};

	fileSystems = {
		"/mnt/SDD" = {
			device = "/dev/disk/by-label/SDD"; # Actual device is randomised for some reason
			fsType = "ntfs";
			options = [
				"allow_other"
				"x-systemd.automount"
			];
		};
		"/mnt/hekate" = {
			device = "u3836@192.168.0.203:/";
			fsType = "sshfs";
			options = [
				"identityfile=/home/u3836/.ssh/medusa"
				"idmap=user"
				"x-systemd.automount"
				"allow_other"
				"user"
			];
		};
		"/mnt/eurydice" = {
			device = "root@192.168.0.200:/";
			fsType = "sshfs";
			options = [
				"identityfile=/home/u3836/.ssh/medusa"
				"idmap=user"
				"x-systemd.automount"
				"allow_other"
				"user"
			];
		};
		"/mnt/orpheus" = {
			device = "u3836@192.168.0.202:/";
			fsType = "sshfs";
			options = [
				"identityfile=/home/u3836/.ssh/medusa"
				"idmap=user"
				"x-systemd.automount"
				"allow_other"
				"user"
			];
		};
	};

	networking.hostName = "medusa";

	systemd.tmpfiles.rules = [
		"L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
	];

	services = {
		xserver = {
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

		ddccontrol.enable = true;
		journald.extraConfig = "SystemMaxUse=256M";
		fwupd.enable = true;
		flatpak.enable = flatpak;

		ollama = {
			acceleration = "rocm";
			environmentVariables = {
				HCC_AMDGPU_TARGET = "gfx1100";
			};
			rocmOverrideGfx = "11.0.0";
		};
	};

	users.groups = {
		i2c = {};
		plugdev = {};
	};

	system.stateVersion = "22.11";
}
