{ pkgs, config, ... }:

let strata-service = {name, image, env, port ? 8080}: {
		description = "Strata inference server";
		wantedBy = [ "multi-user.target" ];
		after = [ "docker.service" "network-online.target" ];
		requires = [ "docker.service" ];

		serviceConfig = {
			Type = "simple";

			ExecStartPre = [
				"-${pkgs.docker}/bin/docker rm -f ${name}-strata"
				"${pkgs.docker}/bin/docker pull ${image}"
			];

			ExecStart = ''
				${pkgs.docker}/bin/docker run                                  \
					--rm                                                       \
					--name ${name}-strata                                      \
					--device=/dev/kfd                                          \
					--device=/dev/dri                                          \
					--group-add=${toString config.ids.gids.video}              \
					--group-add=${toString config.ids.gids.render}             \
					--publish=0.0.0.0:${toString port}:8080                    \
					--ulimit=memlock=-1:-1                                     \
					--shm-size=2g                                              \
					--stop-timeout=120                                         \
					--volume=strata-data:/data                                 \
					--volume=/var/cache/strata/models:/data/Strata-data/models \
					--volume=/var/cache/strata/packs:/data/Strata-data/packs   \
					--volume=/var/cache/strata/mtp:/data/Strata-data/mtp       \
					--env=API_KEY=none                                         \
					${env} ${image}                                            \
			'';

			ExecStop = "-${pkgs.docker}/bin/docker stop --time 120 ${name}-strata";

			Restart = "on-failure";
			RestartSec = "5s";
			TimeoutStopSec = "150s";
		};
	};
in {
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

	systemd.services.strata-qwen-coder = strata-service {
		name = "qwen";
		image = "ghcr.io/niko1221/strata:0.2.21-rocm-gfx110x";
		env = ''
			--env=FAMILY=coder                     \
			--env=MODEL=IQ1_M                      \
			--env=DRAFTING=mtp                     \
			--env=STRATA_SET=vram_reserve_mib=4096 \
		'';
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
