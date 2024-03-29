{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		nixos-hardware.url = "github:NixOS/nixos-hardware";
	};
	outputs = { self, nixpkgs, nixos-hardware }: {
		nixosConfigurations.eurydice = nixpkgs.lib.nixosSystem {
			system = "aarch64-linux";
			modules = [
				../eurydice.nix
				nixos-hardware.nixosModules.raspberry-pi-4
			];
		};
	};
}
