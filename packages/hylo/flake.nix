{
	description = "The Hylo programming language";
	inputs = {
		nixpkgs.url     = "github:stephank/nixpkgs/feat/swift-5.9";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem(system:
		let pkgs = nixpkgs.legacyPackages.${system};
		in rec {
			packages.default = packages.hylo;
			packages.hylo =
			pkgs.stdenv.mkDerivation rec {
				pname = "hylo";
				version = "240313";

				src = pkgs.fetchFromGitHub {
					owner = "hylo-lang";
					repo = "hylo";
					rev = "2c4594919cbb958cf75a511f36ccc89b604178b6";
					sha256 = "sha256-nujSTxs54YhvmBLyhI04buXFd8TeqKn2sdL5rJA7vBA=";
				};

				nativeBuildInputs = [ pkgs.llvmPackages_15.libllvm ];
				buildInputs = with pkgs.swiftPackages; [
					swift
					swift-driver
					swiftpm
					Foundation
					stdenv
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
