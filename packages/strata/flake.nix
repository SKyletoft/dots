{
  description = "Nix packaging for Strata (Qwen3.8-Flash-Next on a single AMD RX 7900 XTX + system RAM)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    strata-src = {
      url   = "github:Niko1221/Strata/v0.2.11";
      flake = false;
    };
  };

  outputs = { nixpkgs, strata-src, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      # llama.cpp at the commit Strata pins (setup.py LLAMA_CPP_COMMIT)
      llamaCppSrc = pkgs.fetchFromGitHub {
        owner = "ggml-org";
        repo  = "llama.cpp";
        rev   = "3cf03257f219afbe7334045ff7c6a06ac68c627d";
        hash  = "sha256-SRGoXa+4ACBCB3eaG9XFYhMN1i0FyPEy9Rrer+dFGYI=";
      };

      # Python environment matching requirements.txt
      pythonEnv = pkgs.python3.withPackages (ps: with ps; [
        numpy jinja2 regex pyyaml tqdm requests pillow psutil truststore textual
      ]);

      # ROCm packages built for gfx1100 only (RX 7900 XT / XTX)
      rocmPackages = pkgs.rocmPackages.gfx1100;
      rocmLibDirs = [
        "${rocmPackages.clr}/lib"
        "${rocmPackages.rocm-runtime}/lib"
        "${rocmPackages.rocm-comgr}/lib"
        "${rocmPackages.hipblas}/lib"
        "${rocmPackages.rocblas}/lib"
        "${rocmPackages.hipblaslt}/lib"
      ];

      engineBuildJson = builtins.toJSON {
        version  = "0.2.11";
        archs    = [ "gfx1100" ];
        backend  = "hip";
        source   = "nix";
        vision   = "none";
        lib_dirs = rocmLibDirs;
      };

      strata-engine = pkgs.callPackage ./pkgs/strata-engine.nix {
        inherit strata-src llamaCppSrc rocmPackages;
      };

      strata-server = pkgs.callPackage ./pkgs/strata-server.nix {
        inherit strata-src pythonEnv strata-engine llamaCppSrc engineBuildJson;
      };

    in {
      # nix run github:hinriksnaer/strata-nix -- \
      #   --family qwen --model IQ2_XS --gpu 0 --port 8080 --data-dir /path
      packages.${system}.default = strata-server;
    };
}
