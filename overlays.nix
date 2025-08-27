nativeBuild: nativeArch:
[
	(self: super: {
		cascadia-code-greek = super.cascadia-code.overrideAttrs(old: {
			url = "";
		});
	})
	(final: prev: {
		lorri = prev.lorri.overrideAttrs(old: {
			src = prev.fetchFromGitHub {
				owner = "kini";
				repo = old.pname;
				rev = "6fe667e5d63e6e347b545c9f28c9da15a7c383e0";
				sha256 = "sha256-bBxXhN09fhUv8BT/bm2n2CGjwA+AUnbXSSW0UE8dqTg=";
			};
		});
	})
] ++ (if nativeBuild then [
	(final: prev: {
		linuxPackages_xanmod = prev.linuxPackages_xanmod.extend (kfinal: kprev: {
			kernel = (kprev.kernel.override {
				name = "linux-xanmod-native";
				extraMakeFlags = [
					"KCFLAGS+=-O3"
					("KCFLAGS+=-march=" + nativeArch)
					("KCFLAGS+=-mtune=" + nativeArch)
				];
			});
		});
	})
] else [])
