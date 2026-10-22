{ pkgs, ... }:

let
	llama-cpp-rocm-0_7_0 = pkgs.llama-cpp-rocm.overrideAttrs (oldAttrs: {
		version = "0.7.0";
		src = pkgs.fetchFromGitHub {
			owner = "ggml-org";
			repo = "llama.cpp";
			tag = "v0.7.0";
			hash = "sha256-pEDYgI3AMTU+cekvRX7NNt5wddznydQSNuerswvEpsA=";
		};
		# 0.6.0 patch (07ccce2) is already merged upstream in 0.7.0.
		patches = [ ];
		npmDepsHash = "sha256-hqYzdp1FwPhYkBP278fQXgY9NCKB6Q3P6Gko7BfP0m0=";
		# NOTE: LLAMA_BUILD_NUMBER (upstream 11669) / LLAMA_BUILD_COMMIT
		# (upstream 620d222) are let-bound in nixpkgs, not overridable via
		# overrideAttrs; the stale values are informational only
		# (`llama-server --version`, /props, web UI).
	});
in
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
			enable = true;
			package = llama-cpp-rocm-0_7_0;
			settings =
				let use-mtp = true;
				in {
					host = "127.0.0.1";
					port = 8080;

					hf-file = "Qwen3.6-35B-A3B-UD-Q2_K_XL.gguf";

					gpu-layers = "all";

					cpu-moe = true;
					moe-cache-mib = 8192;

					flash-attn = "on";
					cache-type-k = "q8_0";
					cache-type-v = "q8_0";
					ctx-size = 128000;

					sleep-idle-seconds = 120;
				} // (if use-mtp then {
					hf-repo = "unsloth/Qwen3.6-35B-A3B-MTP-GGUF";
					spec-type = "draft-mtp";
					spec-draft-n-max = 1;
				} else {
					hf-repo = "unsloth/Qwen3.6-35B-A3B-GGUF";
				});
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
