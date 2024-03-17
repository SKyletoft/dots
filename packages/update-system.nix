{ pkgs }:

pkgs.writeShellScript "update-system" ''
	export PATH=${pkgs.lib.strings.makeBinPath [ pkgs.git ]}
	set -e
	cd /etc/nixos/dots
	git reset --hard
	git fetch origin
	if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/master)" ]; then
		git pull origin master
		nixos-rebuild switch
	fi
''
