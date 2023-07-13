{ lib
, stdenvNoCC
, fetchzip
}:

stdenvNoCC.mkDerivation rec {
  pname = "0xProto";
  version = "1.001";

  src = fetchzip {
    url = "https://github.com/0xType/0xProto/releases/download/1.001/0xProto_1_001.zip";
    sha256 = "sha256-jbtqJK8bCC3viatYi1hMfbJh/TulDdoB+IyK8wQT/kQ=";
  };

  installPhase = ''
	mkdir -p $out/share/fonts/opentype
	mkdir -p $out/share/fonts/truetype

	ls

	cp *.otf $out/share/fonts/opentype
	cp *.ttf $out/share/fonts/truetype
  '';

  meta = with lib; {
	description = "Free and Open-source font for programming.";
    homepage = "https://github.com/0xType/0xProto";
    changelog = "https://raw.githubusercontent.com/0xType/0xProto/${version}/CHANGELOG.md";
    license = licenses.ofl;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
