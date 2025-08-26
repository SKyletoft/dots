{ pkgs }:

pkgs.writeShellScriptBin "update-system" ''
	export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ git nixos-rebuild nix-output-monitor systemd ]) }
	set -e
	cd /etc/nixos/dots
	git fetch origin
	if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/master)" ]; then
		git reset --hard origin/master
		nixos-rebuild switch -L --json |& nom
	fi
''
