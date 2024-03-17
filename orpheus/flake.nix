{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		nixos-hardware.url = "github:NixOS/nixos-hardware";
	};
	outputs = { self, nixpkgs, nixos-hardware }: {
		nixosConfigurations = {
			orpheus = nixpkgs.lib.nixosSystem {
				system = "aarch64-linux";
				modules = [
					../orpheus.nix
					nixos-hardware.nixosModules.raspberry-pi-4
				];
			};
			nixos = nixpkgs.lib.nixosSystem {
				system = "aarch64-linux";
				modules = [
					../pi-default.nix
					nixos-hardware.nixosModules.raspberry-pi-4
				];
				networking.hostName = "orpheus";
			};
		};
	};
}
