{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		stablePkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
		vimPin.url = "github:NixOS/nixpkgs?rev=9813adc7f7c0edd738c6bdd8431439688bb0cb3d";
		emacsPkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
		emacsOverlay = {
			url = "github:nix-community/emacs-overlay?rev=cb16f015f4dd579cf5bd00d09e6a7ada6e72f5ab";
			inputs.nixpkgs.follows = "emacsPkgs";
		};
		home-manager = {
			url = "github:nix-community/home-manager";
			inputs.nixpkgs.follows = "nixpkgs";
		};
		upwards.url = "github:SKyletoft/upwards";
		lem.url = "github:dariof4/lem-flake";
	};

	outputs = {self, nixpkgs, home-manager, ...}@inputs: {
		defaultPackage.x86_64-linux = home-manager.defaultPackage.x86_64-linux;
		defaultPackage.aarch64-linux = home-manager.defaultPackage.aarch64-linux;

		homeConfigurations."u3836" = home-manager.lib.homeManagerConfiguration {
			pkgs = nixpkgs.legacyPackages.${builtins.currentSystem};
			extraSpecialArgs = { inherit inputs; };
			modules = [
				../home.nix
				{ nixpkgs.config.allowUnfreePredicate = _: true; }
			];
		};
	};
}
