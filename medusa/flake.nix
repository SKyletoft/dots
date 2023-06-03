{
	inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
	outputs = { self, nixpkgs, nixos-hardware }: {
		nixosConfigurations.medusa = nixpkgs.lib.nixosSystem {
			system = "x86_64-linux";
			modules = [ ../medusa.nix ];
		};
	};
}
