{ lib
, stdenvNoCC
, fetchzip
}:

stdenvNoCC.mkDerivation rec {
  pname = "monaspace";
  version = "1.000";

  src = fetchzip {
	url = "https://github.com/githubnext/${pname}/releases/download/v${version}/${pname}-v${version}.zip";
    sha256 = "sha256-H8NOS+pVkrY9DofuJhPR2OlzkF4fMdmP2zfDBfrk83A=";
	stripRoot = false;
  };

  installPhase = ''
	mkdir -p $out/share/fonts/opentype
	mkdir -p $out/share/fonts/truetype

	ls

	cp ${pname}-v${version}/fonts/otf/*.otf $out/share/fonts/opentype
	cp ${pname}-v${version}/fonts/variable/*.ttf $out/share/fonts/truetype
  '';

  meta = with lib; {
	description = "An innovative superfamily of fonts for code";
    homepage = "https://github.com/githubnext/monaspace";
    changelog = "https://raw.githubusercontent.com/githubnext/${pname}/v${version}/CHANGELOG.md";
    license = licenses.ofl;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
