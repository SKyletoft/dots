{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		nixos-hardware.url = "github:NixOS/nixos-hardware";
		nixGL.url = "github:nix-community/nixGL";
	};
	outputs = { self, nixpkgs, nixos-hardware, nixGL }@inputs: {
		nixosConfigurations.medea = nixpkgs.lib.nixosSystem {
			system = "x86_64-linux";
			inherit inputs;
			modules = [
				../medea.nix
				nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga-7th-gen
			];
		};
	};
}
