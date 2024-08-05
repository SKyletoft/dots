{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		stablePkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
	};
	outputs = { self, nixpkgs, stablePkgs, nixos-hardware }@inputs: {
		nixosConfigurations.medusa = nixpkgs.lib.nixosSystem {
			system = "x86_64-linux";
			modules = [ ../medusa.nix ];
		};
	};
}
