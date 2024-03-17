{
	description = "A very basic flake";
	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem(system:
		let pkgs = nixpkgs.legacyPackages.${system};
		in rec {
			packages.default = packages.lem;
			packages.lem =
				pkgs.stdenv.mkDerivation rec {
					pname = "lem";
					version = "2.2.0";

					src = pkgs.fetchFromGitHub {
						owner = "lem-project";
						repo = "lem";
						rev = "e366bda73b7e5263cf9ba19678f9b958df48332b";
						sha256 = "sha256-aMPyeOXyFSxhh75eiAwMStLc2fO1Dwi2lQsuH0IYMd0=";
					};

					nativeBuildInputs = with pkgs; [
						sbclPackages.qlot
					];
					buildInputs = with pkgs; [
						sbcl
					];

					buildPhase = ''
						make sdl2
					'';

					installPhase = ''
						mkdir -p $out/bin
						ls
						ls $out
					'';
				};

		});
}
