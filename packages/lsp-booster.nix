{ stdenv
, fetchurl
, unzip
}:

stdenv.mkDerivation rec {
	pname = "lsp-booster";
	version = "v0.1.1";

	src = fetchurl {
		url = "https://github.com/blahgeek/emacs-lsp-booster/releases/download/${version}/emacs-lsp-booster_${version}_x86_64-unknown-linux-musl.zip";
		sha256 = "sha256-89qw75HHNtK9+fvonoHZ0gCjeGoyKm69/yZIAD15RPI=";
	};

	unpackPhase = ''
		mkdir emacs-lsp-booster
		cd emacs-lsp-booster
		${unzip}/bin/unzip $src
	'';

	installPhase = ''
		mkdir -p $out/bin
		mv emacs-lsp-booster $out/bin
	'';
}
