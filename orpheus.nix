{ config, pkgs, lib, ... }:

{
	networking.hostName = "orpheus";

	hardware = {
		graphics.enable = false;
		pulseaudio.enable = false;
	};

	services = {
		transmission = {
			enable = true;
			openRPCPort = true;
			settings = {
				rpc-bind-address = "0.0.0.0";
				rpc-whitelist = "127.0.0.1,192.168.*.*";
			};
		};
		cron = {
			enable = true;
			systemCronJobs =
				[("* * * * * u3836 "
					+ "${pkgs.neofetch}/bin/neofetch > /tmp/eurydice-status; "
					+ "SYSTEMD_COLORS=true systemctl status mullvad-daemon | head -n3 >> /tmp/eurydice-status; "
					+ "curl https://am.i.mullvad.net/connected >> /tmp/eurydice-status; "
				)];
		};
	};

	system.stateVersion = "21.11";
}
