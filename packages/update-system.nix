{ pkgs }:

pkgs.writeShellScriptBin "update-system" ''
	export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ git nixos-rebuild systemd ]) }
	set -e
	cd /etc/nixos/dots
	git reset --hard HEAD~10
	git fetch origin
	if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/master)" ]; then
		git pull origin master
		nixos-rebuild switch -L
	fi
''
