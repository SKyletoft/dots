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

	networking.hostName = "medea";

	powerManagement.powertop.enable = true;

	services = {
		power-profiles-daemon.enable = false;
		tlp = {
			enable = true;
			settings = {
				RUNTIME_PM_ON_AC="auto";
				RUNTIME_PM_ON_BAT="auto";
				CPU_ENGERY_PERF_POLICY_ON_AC="balance_power";
				CPU_ENGERY_PERF_POLICY_ON_BAT="power";
				WIFI_PWR_ON_AC="on";
				WIFI_PWR_ON_BAT="on";
			};
		};
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
