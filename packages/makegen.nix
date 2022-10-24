{ stdenv, fetchgit }:
stdenv.mkDerivation rec {
	pname = "makegen";
	version = "1.0";

	src = fetchgit {
		url = "https://github.com/SKyletoft/make_gen";
	};

	(builtins.getFlake (toString ./.)).outputs.packages.${builtins.currentSystem}.default
};
