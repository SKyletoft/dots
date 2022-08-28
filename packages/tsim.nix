{ stdenv
, lib
, pkgs
, fetchurl
, libXaw
, libXt
, libXmu
, libX11
, libXext
, jdk11
, sox
}:

stdenv.mkDerivation rec {
	pname = "tsim";
	version = "0.84";

	src = fetchurl {
		url = "https://www.cse.chalmers.se/edu/course/TDA384_LP1/files/labs/tsim-0.84.tgz";
		sha256 = "sha256-MzYsG8vZIVM/F9sOF4BbJp517RJN8hoHCFgIfpOFMjk=";
	};

	buildInputs = [
		libXaw
		libXt
		libXmu
		libX11
		libXext
		sox
		jdk11
	];

	meta = with lib; {
		homepage = "https://www.cse.chalmers.se/edu/course/TDA384_LP1/resources/";
		description = "Train Sim for TDA384";
		platforms = [
			"x86_64-linux"
			"aarch64-linux"
			"x86_64-darwin"
			"aarch64-darwin"
		];
		license = licenses.gpl3;
	};
}
