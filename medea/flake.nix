{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		nixos-hardware.url = "github:NixOS/nixos-hardware";
		nixGL.url = "github:nix-community/nixGL";
	};
	outputs = { self, nixpkgs, nixos-hardware, nixGL }@args: {
		nixosConfigurations.medea = nixpkgs.lib.nixosSystem {
			system = "x86_64-linux";
			specialArgs = args // {
				waylandSupport = true;
				windowsFonts = false;
				nativeBuild = false;
			};
			modules = [
				../medea.nix
				../common-system.nix
				./hardware-configuration.nix
				nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga-7th-gen
			];
		};
	};
}
