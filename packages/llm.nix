{ lib, stdenv, fetchurl, buildFHSEnv }:

let llm = stdenv.mkDerivation rec {
		pname = "wizardcoder";
		version = "v1.0.Q5_K_M";

		src = fetchurl {
			url = "https://huggingface.co/jartine/WizardCoder-Python-34B-V1.0-llamafile/resolve/main/wizardcoder-python-34b-v1.0.Q5_K_M.llamafile?download=true";
			sha256 = "cb5aea4f0fc17ed1706edde33d8e628866c259a6ddf447c627026e64da0b9318";
			# url = "https://huggingface.co/jartine/phi-2-llamafile/resolve/main/phi-2.Q5_K_M.llamafile?download=true";
			# sha256 = "sha256-S+SxXK8hOGBhEVZJ7rejMxGL47eeF1VfnD3UQJDVMBw=";
		};

		# Unpacking has to produce *something*
		unpackCmd = "mkdir foo";

		installPhase = ''
			mkdir -p $out/bin
			cp $src $out/bin/wizardcoder-llm
			chmod +x $out/bin/wizardcoder-llm
		'';
	};
in buildFHSEnv {
	name = "llm";
	targetPkgs = pkgs: with pkgs; [ llm ];
	runScript = "wizardcoder-llm";
}
