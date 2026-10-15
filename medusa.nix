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

		# ollama = {
		#	package = pkgs.ollama-rocm;
		#	environmentVariables = {
		#		OLLAMA_MAX_LOADED_MODELS = "1";
		#		OLLAMA_GPU_MEMORY_FRACTION = "1.0";
		#		OLLAMA_CONTEXT_LENGTH = "30000";
		#	};
		# };
		llama-cpp = {
			enable = true;
			package = llama-cpp-rocm-0_7_0;
			settings = {
				host = "127.0.0.1";
				port = 8080;
				hf-repo = "unsloth/Qwen3.6-35B-A3B-GGUF";
				hf-file = "Qwen3.6-35B-A3B-UD-Q5_K_M.gguf";
				gpu-layers = -1;
				cpu-moe = true;
				flash-attn = "on";
				ctx-size = 32768;
				temp = 0.7;
				top-p = 0.80;
				top-k = 20;
				min-p = 0.0;
				sleep-idle-seconds = 120;
				ngl = 99;
				cmoe = true;
				moe-cache-mib = 8000;
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
