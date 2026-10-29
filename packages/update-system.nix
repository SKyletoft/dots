{ pkgs }:

pkgs.writeShellScriptBin "update-system" ''
	export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ git nixos-rebuild nix-output-monitor systemd nix-output-monitor ]) }
	set -e
	cd /etc/nixos/dots
	git fetch origin
	if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/master)" ]; then
		git reset --hard origin/master
		nixos-rebuild boot -L |& nom && nixos-rebuild switch
	fi
''
