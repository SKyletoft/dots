{ pkgs, ... }:

{
	nixpkgs.config.packageOverrides = pkgs: {
		vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
	};

	boot.kernelParams = [ "i915.force_probe=46a6" ];

	hardware = {
		graphics = {
			enable = true;
			extraPackages = with pkgs; [
				intel-media-driver
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
		power-profiles-daemon.enable = false;
		tlp.enable = true;
		thermald.enable = true;
		printing.enable = true;
	};

	environment.sessionVariables.XCURSOR_THEME = "severa_cursors_2x";

	programs.gamescope.args = [
		"--backend"
		"sdl"
		"-Sinteger"
		"-w1920"
		"-h1200"
		"-f"
		"-e"
	];

	system.stateVersion = "21.11";
}
