{ curl
, dbus
, expat
, fetchurl
, fetchgit
, lib
, libX11
, libxcb
, libXScrnSaver
, libXcomposite
, libXcursor
, libXdamage
, libXext
, libXfixes
, libXi
, libXrandr
, libXrender
, libXtst
, libdrm
, libnotify
, libpulseaudio
, libuuid
, libxshmfence
, ncurses5
, stdenv
, autoPatchelfHook
, wrapGAppsHook
, lttng-ust_2_12
, python310Packages
, pkgs
, tree
}:

stdenv.mkDerivation rec {
	pname = "vimspector";
	version = "2907197660";

	src = fetchurl {
		url = "https://github.com/puremourning/vimspector/releases/download/3215563254/vimspector-linux-3215563254.tar.gz";
		sha256 = "sha256-rhhTgQYJ66yeIcLR0awY6hcwDAPj3p2imjMkVJJlqZk=";
	};

	unpackCmd = ''
		mkdir vimspector
		tar xf $src
		# python3 $out/install_gadget.py --force-enable-java
	'';

	installPhase = ''
		tar xf $src
		mkdir -p $out
		cp -r vimspector/opt/vimspector/* $out

		cd $out
		tree
		mkdir -p gadgets/linux

		rm -rf gadgets/linux/vscode-cpptools

		ln -s \
			${pkgs.vscode-extensions.ms-vscode.cpptools}/share/vscode/extensions/ms-vscode.cpptools
			gadgets/linux/vscode-cpptools

		# python3 $out/install_gadget.py --enable-c --enable-rust --enable-bash --enable-python
		echo "\n\n\n\n PATHS"
		echo $out
		# echo "${pkgs.vscode-extensions.ms-vscode.cpptools}"
		# echo "\n\n\n\n"
	'';

	nativeBuildInputs = [
		autoPatchelfHook
		python310Packages.python
		python310Packages.setuptools
		python310Packages.flake8
	];

	buildInputs = [
		curl
		dbus
		expat
		libX11
		libXScrnSaver
		libXcomposite
		libXcursor
		libXdamage
		libXext
		libXfixes
		libXi
		libXrandr
		libXrender
		libXtst
		libdrm
		libnotify
		libuuid
		libxcb
		libxshmfence
		lttng-ust_2_12
		ncurses5
		stdenv.cc.cc.lib
	];

	runtimeDependencies = [
		pkgs.vscode-extensions.ms-vscode.cpptools
	];

	meta = with lib; {
		homepage = "https://github.com/puremourning/vimspector";
		description = "vimspector - A multi language graphical debugger for Vim";
		platforms = [ "x86_64-linux" ];
		license = licenses.asl20;
	};
}

