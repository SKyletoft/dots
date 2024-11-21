{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		stablePkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
		curaPkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
		citraPkgs.url = "github:NixOS/nixpkgs?rev=1cba04796fe93e7f657c62f9d1fb9cae9d0dd86e"; # Last commit before it was removed
		# citraPkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
		vimPin.url = "github:NixOS/nixpkgs?rev=9813adc7f7c0edd738c6bdd8431439688bb0cb3d";
		emacsPkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
		emacsOverlay = {
			url = "github:nix-community/emacs-overlay?rev=d3881d1b6f01c5d01d4af9b61026315aeb2367fb";
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

	outputs = {self, nixpkgs, home-manager, ...}@inputs:
	let
		home = homeConfig: home-manager.lib.homeManagerConfiguration {
			pkgs = nixpkgs.legacyPackages.${builtins.currentSystem};
			extraSpecialArgs = { inherit inputs; inherit homeConfig; };
			modules = [
				../home.nix
				{ nixpkgs.config.allowUnfreePredicate = _: true; }
			];
		};
		graphicalHome = home {
			enableHyprland  = false;
			enableGnome     = true;
			enableRiver     = false;
			enableSway      = false;
			enableDebugging = false;
		};
		serverHome = home {
			enableHyprland  = false;
			enableGnome     = false;
			enableRiver     = false;
			enableSway      = false;
			enableDebugging = false;
		};
	in {
		defaultPackage.x86_64-linux = home-manager.defaultPackage.x86_64-linux;
		defaultPackage.aarch64-linux = home-manager.defaultPackage.aarch64-linux;

		homeConfigurations."u3836@eurydice" = serverHome;
		homeConfigurations."u3836@orpheus"  = serverHome;
		homeConfigurations."u3836"          = graphicalHome;
	};
}
