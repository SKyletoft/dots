{
	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs = import nixpkgs { inherit system; };
				lsp-booster = pkgs.rustPlatform.buildRustPackage rec {
					pname = "emacs-lsp-booster";
					version = "v0.2.1";

					src = pkgs.fetchFromGitHub {
						owner = "blahgeek";
						repo = pname;
						rev = version;
						sha256 = "sha256-uP/xJfXQtk8oaG5Zk+dw+C2fVFdjpUZTDASFuj1+eYs=";
					};

					doCheck = false;
					cargoLock.lockFile = "${src}/Cargo.lock";
				};
			in {
				packages.default = lsp-booster;
			}
		);
}
