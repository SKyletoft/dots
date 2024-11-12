{ stdenv
, fetchurl
, unzip
, system
}:

stdenv.mkDerivation rec {
	pname = "lsp-booster";
	version = "v0.2.1";

	src =
		let arch = if system == "x86_64-linux"
			then "x86_64-unknown-linux-musl"
			else "aarch64-unknown-linux-musl";
		in fetchurl {
			url = "https://github.com/blahgeek/emacs-lsp-booster/releases/download/${version}/emacs-lsp-booster_${version}_${arch}.zip";
			sha256 = "f47745e6754eb5a54525da705ba2c83330ba5fbf6613e8ebbe728894fd0b468e";
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
