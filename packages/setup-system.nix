{ pkgs }:

pkgs.writeShellScriptBin "setup-system" ''
	export PATH=${pkgs.lib.strings.makeBinPath (with pkgs; [ git coreutils ])}
	set -e
	cd /etc/nixos
	rm -rf flake.nix dots || true
	git clone https://github.com/SKyletoft/dots
	ln -s dots/$1/flake.nix flake.nix
''
