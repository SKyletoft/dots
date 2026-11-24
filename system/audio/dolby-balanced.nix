{ pkgs, ... }:

let
	dolby = pkgs.runCommand "pipewire-dolby-balanced" {
		passthru.requiredLv2Packages = [ pkgs.lsp-plugins ];
	} ''
		install -Dm644 ${./dolby-balanced.irs} $out/share/pipewire/pipewire.conf.d/Dolby_Balanced.irs
		install -Dm644 ${./dolby-balanced.conf} $out/share/pipewire/pipewire.conf.d/99-dolby-balanced.conf
		substituteInPlace $out/share/pipewire/pipewire.conf.d/99-dolby-balanced.conf \
			--replace-fail "/tmp/opencode/pw/Dolby_Balanced.irs" "$out/share/pipewire/pipewire.conf.d/Dolby_Balanced.irs"
	'';
in {
	services.pipewire.configPackages = [ dolby ];
}
