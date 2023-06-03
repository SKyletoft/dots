{
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		home-manager = {
			url = "github:nix-community/home-manager";
			inputs.nixpkgs.follows = "nixpkgs";
		};
	};

	outputs = {self, nixpkgs, home-manager}: {
		defaultPackage.x86_64-linux = home-manager.defaultPackage.x86_64-linux;

		homeConfigurations = {
			"u3836" = home-manager.lib.homeManagerConfiguration {
				pkgs = nixpkgs.legacyPackages.x86_64-linux;
				modules = [ ../home.nix ];
			};
		};
	};
}
