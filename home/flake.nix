{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		stablePkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
		citraPkgs.url = "github:NixOS/nixpkgs?rev=1cba04796fe93e7f657c62f9d1fb9cae9d0dd86e"; # Last commit before it was removed
		# citraPkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
		vimPin.url = "github:NixOS/nixpkgs?rev=9813adc7f7c0edd738c6bdd8431439688bb0cb3d";
		emacsPkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
		emacsOverlay = {
			url = "github:nix-community/emacs-overlay?rev=b3512b3df5396e17d9e89cadcc3f57db0ea1fecc";
			inputs.nixpkgs.follows = "emacsPkgs";
			inputs.nixpkgs-stable.follows = "emacsPkgs";
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
