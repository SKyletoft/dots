# Generates EasyEffects 8.x output presets, impulse responses and the speaker
# autoload entry for the ThinkPad X1 Yoga Gen 7 (21CD, Realtek ALC287) from
# the Dolby DAX3 tuning inside Lenovo's published audio driver, using
# antoinecellerier/speaker-tuning-to-easyeffects.
{
	pkgs,
	fetchFromGitHub,
	fetchurl,
	runCommand,
	innoextract,
	python3,
}:

let
	tool = fetchFromGitHub {
		owner = "antoinecellerier";
		repo  = "speaker-tuning-to-easyeffects";
		rev   = "v2026.10.1";
		hash  = "sha256-Or0TVael518UQpl073wMdM/YI7EGbDZIUPNuRMw76hw=";
	};

	# AUD_N3AA1 v6.0.9764.1, resolved from Lenovo's update catalog for 21CD
	driver = fetchurl {
		url  = "https://download.lenovo.com/pccbbs/mobiles/n3aa123w.exe";
		hash = "sha256-0oRyF4DQCgoj65xgK24ktJsvKjTt76xjpvCbm+D7xRg=";
	};

	sink = "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__Speaker__sink";
	route = "Speaker";

	pyEnv = python3.withPackages (ps: with ps; [ numpy scipy ]);

	presets = runCommand "easyeffects-dolby-x1yoga7" {
		nativeBuildInputs = [ innoextract pyEnv ];
	} ''
		export HOME=$TMP XDG_CONFIG_HOME=$TMP XDG_DATA_HOME=$TMP
		innoextract -q -d $TMP/drv ${driver}
		xml=$(find $TMP/drv -name 'DEV_0287_SUBSYS_17AA22E6*.xml' ! -name '*_settings.xml' -print -quit)
		test -n "$xml"
		python ${tool}/dolby_to_easyeffects.py "$xml" --all-profiles \
			--output-dir $out/output --irs-dir $out/irs \
			--skip-ee-check --skip-closing --no-color --no-reload
		# The tool only writes the bypass preset with --autoload (needs a live
		# pw-dump). Its content is trivial.
		cat > $out/output/Nothing.json <<EOF
{
    "output": {
        "blocklist": [],
        "plugins_order": []
    }
}
EOF
	'';

	autoload = pkgs.writeText "easyeffects-dolby-autoload" (builtins.toJSON {
		device             = sink;
		device-description = "Alder Lake PCH-P High Definition Audio Controller Speaker";
		device-profile     = route;
		preset-name        = "Dolby-Dynamic-Balanced";
	});
in {
	inherit presets autoload sink route;
}
