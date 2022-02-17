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
	pname = "eterm8";
	version = "1.0";

	src = fetchurl {
		url = "http://gbgmv.se/dl/linux/eterm8_1.0.tar.gz";
		sha256 = "r6pb4o4XiZLZSGcJUQA/E67wnzy18A+rZSCCco2ipC0=";
	};

	unpackCmd = ''
		mkdir eterm8
		tar xf $src --directory=eterm8
	'';

	installPhase = ''
		mkdir -p $out
		mkdir -p $out/bin
		cp -r * $out
		ln -s $out/eterm8 $out/bin/eterm8
		ln -s $out/simserver $out/bin/simserver
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
		description = "Eterm8 integrerar en enkel utvecklingsmiljö för programutveckling i assemblerspråk";
		platforms = [ "x86_64-linux" ];
		license = licenses.unfree;
	};
}

