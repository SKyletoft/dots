{ lib, pkgs, stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
	pname = "cppfront";
	version = "20230910";

	src = fetchFromGitHub {
		owner = "hsutter";
		repo = "cppfront";
		rev = "ecd37263f9b5a71f5beb18affc275d76ee537f9f";
		sha256 = "sha256-ijXoobSZoIsZLqee3igQhH8JymFG2rDUWA61TyrJWH8=";
	};

	nativeBuildInputs = [ stdenv ];
	buildInputs = [ pkgs.gcc ];

	buildPhase = ''
		g++ ${src}/source/cppfront.cpp -std=c++20 -o cppfront
	'';

	installPhase = ''
		mkdir -p $out/bin
		ls $out
		cp cppfront $out/bin
		cp -r $src/include $out
		echo "c++ -I $out/include/cpp2util.h -std=c++20 \$@" > $out/bin/c++2
		chmod +x $out/bin/c++2
	'';

	meta = with lib; {
		description = "A personal experimental C++ Syntax 2 -> Syntax 1 compiler";
		homepage = "https://github.com/hsutter/cppfront";
		# license = licenses.CC-BY-NC-ND-4.0;
		platforms = platforms.linux;
	};
}
