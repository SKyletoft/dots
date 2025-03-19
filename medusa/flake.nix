{
	inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

	outputs = { self, nixpkgs }@args: {
		nixosConfigurations.medusa = nixpkgs.lib.nixosSystem {
			system = "x86_64-linux";
			specialArgs = args // {
				waylandSupport = false;
				windowsFonts = false;
				nativeBuild = false;
				flatpak = false;
			};
			modules = [
				../medusa.nix
			];
		};
	};
}
