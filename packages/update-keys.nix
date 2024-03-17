{ pkgs }:

pkgs.writeShellScriptBin "update-keys" ''
	mkdir -p ~/.ssh
	cd ~/.ssh

	${pkgs.curl}/bin/curl \
		https://github.com/$(echo $1).keys \
		> authorized_keys
	chmod 700 ~/.ssh
	chmod 644 ~/.ssh/authorized_keys
''
