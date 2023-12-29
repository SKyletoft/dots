{
	description = "The Hylo programming language";
	inputs = {
		nixpkgs.url     = "github:NixOS/nixpkgs/nixos-23.05";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem(system:
		let pkgs = nixpkgs.legacyPackages.${system};
		in rec {
			packages.default = packages.hylo;
			packages.hylo =
			pkgs.stdenv.mkDerivation rec {
				pname = "hylo";
				version = "231226";

				src = pkgs.fetchFromGitHub {
					owner = "hylo-lang";
					repo = "hylo";
					rev = "e07a6d32fc40a6bb8b382e4d8a72aa36c0e1ae51";
					sha256 = "sha256-p4KoItnr/xdb5DZ6sFuWSz/syFPf8zCojsHdic9GfTA=";
				};

				nativeBuildInputs = [ pkgs.llvmPackages_15.libllvm ];
				buildInputs = with pkgs.swiftPackages; [
					swift
					swift-driver
					swiftpm
					Foundation
					pkgs.gcc
					pkgs.stdenv
					pkgs.pkg-config
				];

				buildPhase = ''
					swift build -c release
				'';

				installPhase = ''
					mkdir -p $out/bin
					ls $out
				'';

				meta = with pkgs.lib; {
					description = "The Hylo programming language";
					homepage = "https://github.com/hylo-lang/hylo";
					# license = licenses.CC-BY-NC-ND-4.0;
					platforms = platforms.linux;
				};
			};
		});
}
