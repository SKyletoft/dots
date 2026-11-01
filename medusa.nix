{ pkgs, avalon, ... }:
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
				"identityfile=/root/.ssh/medusa"
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
				"identityfile=/root/.ssh/medusa"
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
				"identityfile=/root/.ssh/medusa"
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
	];

	services = {
		xserver.videoDrivers = [ "amdgpu" ];

		llama-cpp = {
			enable = false;
			package = avalon.packages.${pkgs.stdenv.hostPlatform.system}.rocm;
			settings = {
				host = "127.0.0.1";
				port = 8080;

				hf-repo = "freriadoc/Qwen3.8-Flash-Next-Avalon-GGUF";
				hf-file = "Qwen3.8-Flash-Next-Avalon-IQ3KS-00001-of-00033.gguf";

				ctx-size = 262144;
				batch-size = 9536;
				ubatch-size = 9536;
				flash-attn = "on";
				cache-type-k = "q8_0";
				cache-type-v = "q4_0";
				jinja = true;
				parallel = 1;

				spec-draft-hf = "freriadoc/Qwen3.8-Flash-Next-Avalon-GGUF";
				spec-draft-model = "MTP/mtp-Qwen3.8-Flash-Next-shared-IQ3K-IQ4NL-FR65k.gguf";
				spec-type = "draft-mtp";
				spec-draft-n-max = 2;
				spec-draft-ubatch-size = 256;
				spec-draft-type-k = "q8_0";
				spec-draft-type-v = "q8_0";

				# Leave 4 GiB free on the 24 GiB XTX, capping the server at ~20 GiB.
				fit-target = 4096;

				sleep-idle-seconds = 120;
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
