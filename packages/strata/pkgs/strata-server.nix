# Python server wrapper for Strata.
# Copies the Strata Python source tree from upstream and patches setup.py so
# that all mutable state (engine pointer, run configs, logs) is redirected to
# the user's --data-dir (STRATA_DATA_DIR) rather than the immutable Nix store.
#
# Read-only static files (data/, serve/, tools/, CMakeLists.txt) remain in the
# Nix store under _NIX_STORE_ROOT; setup.py is patched to use that variable for
# those paths.
{ lib, stdenv
, strata-src
, pythonEnv
, strata-engine
, llamaCppSrc
, engineBuildJson
}:

stdenv.mkDerivation {
  pname   = "strata-server";
  version = "0.2.11";

  src = strata-src;

  nativeBuildInputs = [ pythonEnv ];

  dontBuild     = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/strata
    cp -r serve            $out/share/strata/
    cp -r data             $out/share/strata/
    cp -r tools            $out/share/strata/
    cp -r strata_control   $out/share/strata/
    cp -r strata_cli       $out/share/strata/
    cp    setup.py         $out/share/strata/
    cp    strata.py        $out/share/strata/
    cp    CMakeLists.txt   $out/share/strata/
    cp    requirements*.txt $out/share/strata/

    # Patch setup.py: redirect mutable state to STRATA_DATA_DIR, no-op pip,
    # return the Nix-provided llama.cpp and engine.
    python3 ${./patch-setup-py.py} "$out/share/strata/setup.py" "${llamaCppSrc}"

    mkdir -p $out/bin

    # strata-server: run setup.py (downloads model if needed, then starts server).
    #
    # On first run (or after a Nix store update) the wrapper initialises the
    # engine directory inside --data-dir so that setup.py finds the pre-built
    # HIP engine without attempting a download or compile:
    #   $STRATA_DATA_DIR/engine/strata        -> symlink to Nix store binary
    #   $STRATA_DATA_DIR/engine/strata-device -> symlink to Nix store binary
    #   $STRATA_DATA_DIR/engine/BUILD.json    -> metadata for setup.py
    #
    # BUILD.json fields:
    #   version  : must be >= MIN_ENGINE (0.1.46)
    #   archs    : ["gfx1100"] = AMD RX 7900 XT / XTX
    #   source   : "nix" (patched setup.py never replaces it)
    #   backend  : "hip"
    #   lib_dirs : absolute Nix ROCm library folders
    cat > $out/bin/strata-server <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

# Parse --data-dir from the argument list; fall back to $HOME/.local/share/strata
_data_dir=""
_prev=""
for _a in "$@"; do
  [ "$_prev" = "--data-dir" ] && _data_dir="$_a"
  _prev="$_a"
done
if [ -z "$_data_dir" ]; then _data_dir="$HOME/.local/share/strata"; fi
export STRATA_DATA_DIR="$_data_dir"

# Initialise the engine directory in the data dir on first run or after update.
_engine_dir="$STRATA_DATA_DIR/engine"
_build_json='@buildJson@'
mkdir -p "$_engine_dir"
# Refresh the symlinks whenever the Nix store path changes (e.g. after flake update).
for _x in strata strata-device; do
  _src="@strata-engine@/bin/$_x"
  [ -e "$_src" ] || continue
  if [ "$(readlink "$_engine_dir/$_x" 2>/dev/null)" != "$_src" ]; then
    ln -sf "$_src" "$_engine_dir/$_x"
  fi
done
# Always refresh: an older CUDA install may have left a BUILD.json behind.
printf '%s\n' "$_build_json" > "$_engine_dir/BUILD.json"

export PATH="/usr/bin:/usr/local/bin:$PATH"

# Raise the locked-memory limit so the engine can pin the embedding matrix
# (~322 MiB for IQ3_S). The default ulimit -l of 8 MiB causes:
#   "native embedding: cannot pin 322 MiB, nor place it in VRAM"
ulimit -l unlimited 2>/dev/null || true

exec @python@/bin/python @share@/setup.py "$@"
EOF
    substituteInPlace $out/bin/strata-server \
      --replace '@strata-engine@' "${strata-engine}" \
      --replace '@share@'         "$out/share/strata" \
      --replace '@python@'        "${pythonEnv}" \
      --replace '@buildJson@'     '${engineBuildJson}'
    chmod +x $out/bin/strata-server

    runHook postInstall
  '';

  meta = with lib; {
    description = "Strata server: OpenAI-compatible LLM inference for Qwen3.8-Flash-Next (AMD HIP)";
    platforms   = [ "x86_64-linux" ];
  };
}
