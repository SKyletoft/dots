{ curl
, dbus
, expat
, fetchurl
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
, python3
, pythonPackages
}:

stdenv.mkDerivation rec {
	pname = "vimspector";
	version = "2907197660";

	src = fetchurl {
		url = "https://github.com/puremourning/vimspector/releases/download/2907197660/vimspector-linux-2907197660.tar.gz";
		sha256 = "sha256-OITuzzG9zlYt9Ku68tIeJ5Sasggp1W+Dm2kM3CksOU8=";
	};

	unpackCmd = ''
		mkdir vimspector
		tar xf $src
	'';

	installPhase = ''
		tar xf $src
		mkdir -p $out
		cp -r vimspector/opt/vimspector/* $out
		python3 $out/install_gadget.py --enable-c --enable-rust --enable-bash --enable-python
	'';

	nativeBuildInputs = [ autoPatchelfHook python3 pythonPackages.setuptools ];

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

	runtimeDependencies = [];

	meta = with lib; {
		homepage = "https://github.com/puremourning/vimspector";
		description = "vimspector - A multi language graphical debugger for Vim";
		platforms = [ "x86_64-linux" ];
		license = licenses.asl20;
	};
}

