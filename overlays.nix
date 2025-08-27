nativeBuild: nativeArch:
[] ++ (if nativeBuild then [
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
