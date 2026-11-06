{ pkgs, strata, ... }:
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
		"L+    /opt/rocm                -    -    -     -    ${pkgs.rocmPackages.clr}"
	];

	services.xserver.videoDrivers = [ "amdgpu" ];

	users.users.strata.isNormalUser = true;
	systemd.services.strata = {
		description = "Strata inference server";
		wantedBy = [ "multi-user.target" ];

		serviceConfig = {
			Type = "simple";
			Environment = [ "STRATA_EXECV=1" ];
			ExecStart = ''
				mkdir -p /var/cache/strata
				${strata.packages.x86_64-linux.default}/bin/strata-server
					--data-dir /var/cache/strata
					--family coder
					--port 8080
					--vram-reserve 4096
					--mtp on
					--drafter dflash2
					--yes
			'';
			Restart = "on-failure";
			RestartSec = "5s";
			User = "strata";
		};
	};

	environment = {
		systemPackages = with pkgs; [ nvtopPackages.amd ];
		sessionVariables.XCURSOR_THEME = "severa_cursors_linux_expanded";
	};

	users.groups = {
		i2c = {};
		plugdev = {};
	};

	system.stateVersion = "22.11";
}
