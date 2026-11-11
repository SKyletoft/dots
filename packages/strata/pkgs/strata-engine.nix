# C++/HIP engine binary for Strata.
# Fetches llama.cpp at the exact commit Strata pins and injects it into the
# source tree so CMake's FetchContent is never invoked during the build.
{ cmake, ninja, pkg-config
, stdenv
, rocmPackages
, strata-src
, llamaCppSrc
}:

let
  # Host codegen target, pinned to medusa's Zen 4 CPU (Ryzen 9 7950X) so the
  # build never depends on the compiling machine's ISA probe.
  cpuFlags = "-w -march=znver4 -mtune=znver4";
  hipIncludes = "-I${rocmPackages.hipblas}/include -I${rocmPackages.hipblas-common}/include";
in stdenv.mkDerivation {
  pname   = "strata-engine";
  version = "0.2.13";

  src = strata-src;

  nativeBuildInputs = [ cmake ninja pkg-config ];

  buildInputs = with rocmPackages; [
    clr
    hipblas
    rocblas
    hipblaslt
    rocm-runtime
    rocm-comgr
  ];

  NIX_CFLAGS_COMPILE   = "${cpuFlags} ${hipIncludes}";
  NIX_CXXFLAGS_COMPILE = "${cpuFlags} ${hipIncludes}";
  CFLAGS               = "${cpuFlags} ${hipIncludes}";
  CXXFLAGS             = "${cpuFlags} ${hipIncludes}";

  # STRATA_GGML_DIR must be an absolute path -- CMake resolves relative paths
  # in add_subdirectory() against the build dir, not the source dir.
  cmakeFlags = [
    "-DSTRATA_ENABLE_HIP=ON"
    "-DSTRATA_ENABLE_CUDA=OFF"
    "-DSTRATA_BUILD_TESTS=OFF"
    "-DSTRATA_PREFILL_MMQ=ON"
    "-DSTRATA_Q6K_EXPERTS=ON"
    "-DSTRATA_MMQ_KQUANTS=ON"
    "-DSTRATA_GGML_DIR=${llamaCppSrc}"
    "-DCMAKE_HIP_ARCHITECTURES=gfx1100"
    "-DCMAKE_HIP_COMPILER=${rocmPackages.clr.hipClangPath}/clang++"
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    install -Dm755 strata $out/bin/strata
    if [ -e strata-device ]; then
      install -Dm755 strata-device $out/bin/strata-device
    fi
    runHook postInstall
  '';

  meta = {
    description = "Strata engine: MoE inference for Qwen3.8-Flash-Next (AMD HIP, gfx1100)";
    platforms   = [ "x86_64-linux" ];
  };
}
