{ fetchurl
, lib
, stdenv
, pkgs
, autoPatchelfHook
, wrapGAppsHook
}:

stdenv.mkDerivation {
		pname = "digiflisp";
		version = "2.07";

		src = fetchurl {
				url = "http://gbgmv.se/dl/linux/digiflisp_2.07.tar.gz";
				sha256 = "sha256-BwO7iDIgmf/1WDeHNMrtZy+iHIs/Loa14UHX+S9j7pM=";
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

		nativeBuildInputs = [ autoPatchelfHook wrapGAppsHook ];

		buildInputs = with pkgs; [
				glib
				gtk3
				xorg.libX11
				xorg.libXxf86vm
				stdenv.cc.cc.lib
		];

		meta = with lib; {
				homepage = "https://www.gbgmv.se";
				description = "Simulatorn Digiflisp för Windows, Linux och MacOS";
				platforms = [ "x86_64-linux" ];
				license = licenses.unfree;
		};
}
