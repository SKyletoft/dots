# Copied from the Opera web brower. Can absolutely be cut down
{ alsa-lib
, atk
, cairo
, cups
, curl
, dbus
, dpkg
, expat
, fetchurl
, fontconfig
, freetype
, gdk-pixbuf
, glib
, gnome2
, gtk3
, gtk4
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
, mesa
, ncurses5
, nspr
, nss
, pango
, stdenv
, systemd
, at-spi2-atk
, at-spi2-core
, autoPatchelfHook
, wrapGAppsHook
}:

stdenv.mkDerivation rec {
	pname = "digiflisp";
	version = "2.03";

	src = fetchurl {
		url = "http://gbgmv.se/dl/linux/digiflisp_2.03.tar.gz";
		sha256 = "19rsxk91byh8ivx41b96q6pb93zs8728lzxijykm4dknhvbp8fqb";
	};

	unpackCmd = ''
		mkdir digiflisp
		tar xf $src --directory=digiflisp
	'';

	installPhase = ''
		mkdir -p $out
		mkdir -p $out/bin
		cp -r * $out
		ln -s $out/digiflisp $out/bin/digiflisp
	'';

	nativeBuildInputs = [ autoPatchelfHook ];

	buildInputs = [
	    alsa-lib
		at-spi2-atk
		at-spi2-core
		atk
		cairo
		cups
		curl
		dbus
		expat
		fontconfig.lib
		freetype
		gdk-pixbuf
		glib
		gnome2.GConf
		gtk3
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
		mesa
		ncurses5
		nspr
		nss
		pango
		stdenv.cc.cc.lib
	];

	runtimeDependencies = [ gtk3 ];

	meta = with lib; {
		homepage = "https://www.gbgmv.se";
		description = "Simulatorn Digiflisp för Windows, Linux och MacOS";
		platforms = [ "x86_64-linux" ];
		license = licenses.unfree;
	};
}

