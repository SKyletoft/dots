{ pkgs, ... }:

{
	nixpkgs.config.rocmSupport = true;

	boot.kernelModules = [ "amdgpu" "kvm-amd" "i2c-dev" "ntsync" ];

	hardware = {
		graphics.enable = true;
		cpu.amd.updateMicrocode = true;
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
		"d     /opt/rocm/include        -    -    -     -"
		"L+    /opt/rocm/hip            -    -    -     -    ${pkgs.rocmPackages.clr}"
		"L+    /opt/rocm/include/hip    -    -    -     -    ${pkgs.rocmPackages.clr}/include/hip"
		"L+    /opt/rocm/include/hipcub -    -    -     -    ${pkgs.rocmPackages.hipcub}/include/hipcub"
	];

	services = {
		xserver.videoDrivers = [ "amdgpu" ];

		ollama = {
			package = pkgs.ollama-rocm;
			environmentVariables = {
				OLLAMA_MAX_LOADED_MODELS = "1";
				OLLAMA_GPU_MEMORY_FRACTION = "0.85";
				OLLAMA_CONTEXT_LENGTH = "100000";
			};
		};
	};

	environment = {
		systemPackages = with pkgs; [ radeontop ];
		sessionVariables.XCURSOR_THEME = "severa_cursors_linux_expanded";
	};

	users.groups = {
		i2c = {};
		plugdev = {};
	};

	system.stateVersion = "22.11";
}
