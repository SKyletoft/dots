{ lib, stdenv, fetchFromGitHub, kernel }:

stdenv.mkDerivation rec {
	pname = "xpad";
	version = "3.2";

	src = fetchFromGitHub {
		owner = "paroj";
		repo = "xpad";
		rev = "1821d2822783e8c24733b05138e06d9a9f4d309d";
		sha256 = "sha256-+KSv+fDwfzsT/rqonCCbl451LzdtDzGvhP5MrMWpfZg=";
	};

	setSourceRoot = ''
		export sourceRoot=$(pwd)/source
	'';

	nativeBuildInputs = kernel.moduleBuildDependencies;

	makeFlags = kernel.makeFlags ++ [
		"-C"
		"${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
		"M=$(sourceRoot)"
	];

	buildFlags = [ "modules" ];
	installFlags = [ "INSTALL_MOD_PATH=${placeholder "out"}" ];
	installTargets = [ "modules_install" ];

	meta = with lib; {
		description = "xpad kernel module";
		homepage = "https://github.com/paroj/xpad";
		license = licenses.gpl2Plus;
		platforms = platforms.linux;
	};
}
