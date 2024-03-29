{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		stablePkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
		vimPin.url = "github:NixOS/nixpkgs?rev=9813adc7f7c0edd738c6bdd8431439688bb0cb3d";
		emacsOverlay.url = "github:nix-community/emacs-overlay?rev=2be142e8ad86fd395ad4738e2aa66886592db930";
		emacsPkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
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
