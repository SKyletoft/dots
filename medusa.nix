{ config, pkgs, waylandSupport, windowsFonts, nativeBuild, flatpak, ... }:

{
	nixpkgs.config.rocmSupport = true;

	boot.kernelModules = [ "amdgpu" "kvm-amd" "i2c-dev" ];

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

		ollama = {
			acceleration = "rocm";
			environmentVariables = {
				HCC_AMDGPU_TARGET = "gfx1100";
				OLLAMA_GPU_MEMORY_FRACTION = "0.8"; # Only allow 80% VRAM usage
				OLLAMA_GPU_OVERHEAD = "2147483648"; # Leave 2GB for non-llm tasks, yes this conflicts with above
				OLLAMA_CONTEXT_LENGTH = "65536";    # Extend context length to 64k
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
