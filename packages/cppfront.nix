{ lib, pkgs, stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
	pname = "cppfront";
	version = "20230813";

	src = fetchFromGitHub {
		owner = "hsutter";
		repo = "cppfront";
		rev = "6ce2643e3ae7f91d0b21bc574fc321f7f015a466";
		sha256 = "sha256-g2nkAijhzFhM6F43AjFkoV1cqH3mX6Ma36R8CG7yqnc=";
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
