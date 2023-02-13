nativeBuild:
[
	(self: super: {
		cascadia-code-greek = super.cascadia-code.overrideAttrs(old: {
			url = "";
		});
		gnome = super.gnome.overrideScope' (gself: gsuper: {
			mutter = gsuper.mutter.overrideAttrs (oldAttrs: {
				patches = [ ./1441.patch ] ++ oldAttrs.patches;
			});
		});
		wlroots = super.wlroots.overrideAttrs(old: {
			postPatch = "sed -i 's/assert(argb8888 &&/assert(true || argb8888 ||/g' 'render/wlr_renderer.c'";
		});
	})
] ++ (if nativeBuild then [
	(self: super: {
		stdenv = super.impureUseNativeOptimizations super.stdenv;
	})
] else [])
