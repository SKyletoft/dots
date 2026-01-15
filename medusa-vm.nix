{ config, pkgs, lib, ... }:

{
	nix.settings = {
		substituters = [
			"https://192.168.0.200"
		];
		trusted-public-keys = [
			"nix.u3836.se:t7H/bFWi14aBFYPE5A00eEQawd7Ssl/fXbq/2C+Bsrs="
		];
	};

	boot.loader.systemd-boot.enable = true;

	networking.hostName = "medusa-vm";

	hardware.graphics.enable = false;
	services.pulseaudio.enable = false;

	services = {
		transmission = {
			enable = true;
			openRPCPort = true;
			package = pkgs.transmission_4;
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
