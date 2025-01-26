{
	description = "The Hylo programming language";
	inputs = {
		nixpkgs.url     = "github:NixOS/nixpkgs";
		flake-utils.url = "github:numtide/flake-utils";
		swift.url       = "github:SKyletoft/swift-flake";
	};

	outputs = { self, nixpkgs, swift, flake-utils }: flake-utils.lib.eachDefaultSystem(system:
		let pkgs = import nixpkgs {
			inherit system;
			sandbox = false;
		};
		in rec {
			packages = {
				default = packages.hylo;
				hylo =
					pkgs.stdenv.mkDerivation {
						pname = "hylo";
						version = "250124";

						src = pkgs.fetchFromGitHub {
							owner = "hylo-lang";
							repo = "hylo";
							rev = "c322d349fe34f4a9a21b137e980dc8f94a5fc33c";
							sha256 = "sha256-JEWy4d4sjUrszf05QDYVtbqzl5lh6bnBeoV8qCuS7DM=";
						};

						nativeBuildInputs = [ pkgs.llvmPackages_17.libllvm ];
						buildInputs = [
							swift.packages.${system}.swift
							pkgs.stdenv
							pkgs.cmake
							pkgs.ninja
							pkgs.git
						];

						SWIFTC="${swift.packages.${system}.swiftc}/bin/swiftc";
						configurePhase = ''
							cmake -D CMAKE_BUILD_TYPE=Release \
								-D LLVM_DIR=${pkgs.llvmPackages_17.libllvm}/lib/cmake/llvm \
								-G Ninja -S . -B build
						'';

						# CMake wants to clone stuff
						buildPhase = ''
							export NIX_REMOTE=https://github.com
							cmake --build build
						'';
						allowNetwork = true;

						installPhase = ''
							mkdir -p $out/bin
							ls $out
						'';

						meta = with pkgs.lib; {
							description = "The Hylo programming language";
							homepage = "https://github.com/hylo-lang/hylo";
							platforms = platforms.linux;
						};
					};
			};
		});
}
