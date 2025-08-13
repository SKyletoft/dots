{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		nixos-hardware.url = "github:NixOS/nixos-hardware";
		nixGL.url = "github:nix-community/nixGL";
	};

	outputs = { self, nixpkgs, nixos-hardware, nixGL }@args: {
		nixosConfigurations = {

			medusa = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = args // {
					waylandSupport = true;
					windowsFonts   = false;
					nativeBuild    = false;
					flatpak        = false;
				};
				modules = [
					../medusa.nix
					../common-system.nix
					./medusa-hardware.nix
				];
			};

			medea = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = args // {
					waylandSupport = true;
					windowsFonts   = false;
					nativeBuild    = false;
					flatpak        = false;
				};
				modules = [
					../medea.nix
					../common-system.nix
					./medea-hardware.nix
					nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga-7th-gen
				];
			};

			persephone = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = args // {
					waylandSupport = true;
					windowsFonts   = false;
					nativeBuild    = false;
					flatpak        = false;
				};
				modules = [
					../persephone.nix
					../common-system.nix
					./persephone-hardware.nix
					nixos-hardware.nixosModules.microsoft-surface-pro-intel
				];
			};

			eurydice = nixpkgs.lib.nixosSystem {
				system = "aarch64-linux";
				modules = [
					../eurydice.nix
					../common-server.nix
					nixos-hardware.nixosModules.raspberry-pi-4
				];
			};

			orpheus = nixpkgs.lib.nixosSystem {
				system = "aarch64-linux";
				modules = [
					../orpheus.nix
					../common-server.nix
					nixos-hardware.nixosModules.raspberry-pi-4
				];
			};
		};
	};
}
