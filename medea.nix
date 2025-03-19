{ config, pkgs, lib, nixGL, waylandSupport, windowsFonts, nativeBuild, ... }:

{
	nixpkgs = {
		config = {
			packageOverrides = pkgs: {
				vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
			};
		};
	} // (if nativeBuild then {
		localSystem =  {
			gcc.arch = "alderlake";
			gcc.tune = "alderlake";
			system = "x86_64-linux";
		};
	} else {});

	boot.kernelParams = [ "i915.force_probe=46a6" ];

	hardware = {
		graphics = {
			enable = true;
			extraPackages = with pkgs; [
				# intel-compute-runtime
				# intel-ocl
				# ocl-icd
				intel-media-driver
				# vaapiIntel
				vaapiVdpau
				libvdpau-va-gl
				vpl-gpu-rt
			];
		};
		# Camera
		ipu6 = {
			enable = true;
			platform = "ipu6ep";
		};
		cpu.intel.updateMicrocode = true;
	};

	networking = {
		hostName = "medea";
		networkmanager = {
			wifi.powersave = true;
			/*
			# And this imperative hack
			$ cat /etc/modprobe.d/intel_wifi.conf
			options iwlmvm power_scheme=1
			options iwlwifi power_save=Y power_level=5
			*/
		};
	};

	powerManagement.powertop.enable = true;

	services = {
		thermald.enable = true;
		printing.enable = true;
		thinkfan.enable = false;

		fprintd = {
			enable = false;
			tod = {
				enable = false;
				# driver = todo;
			};
		};

		flatpak.enable = false;
		pcscd.enable = true;
	};

	environment = {
		systemPackages = with pkgs; [
			nixGL.packages.${system}.nixGLIntel
		];
	};

	system.stateVersion = "21.11";
}
