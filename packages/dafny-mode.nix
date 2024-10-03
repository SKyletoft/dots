{ trivialBuild
, fetchFromGitHub
}:
trivialBuild rec {
	pname = "dafny-mode";
	version = "241001";
	src = fetchFromGitHub {
		owner = "SKyletoft";
		repo = "boogie-friends";
		rev = "ca902b37e0756c4d69c7dd28afe3070c62821807";
		hash = "";
	};
	propagatedUserEnvPkgs = [];
	buildInputs = propagatedUserEnvPkgs;
}
