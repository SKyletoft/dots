"""
Patch Strata's setup.py for the Nix environment.

Design principle: minimise divergence from upstream. Every patch targets a
stable anchor (function signature, module-level constant) so that upstream
changes to function bodies do not break us.

Patches applied:
  1. ROOT split: mutable state (engine/, configs, logs) -> STRATA_DATA_DIR
                 read-only static files (data/, serve/, tools/) -> _NIX_STORE_ROOT
  2. get_llama_cpp()  -> return pre-fetched Nix store path (no download)
  3. pip_install()    -> no-op (packages pre-baked in pythonEnv)
  4. HIP engine       -> use the Nix-built engine instead of compiling
  5. update checks    -> a Nix-managed engine is never recompiled

Usage: python3 patch-setup-py.py <path-to-setup.py> <llama-cpp-store-path>
"""
import sys
import re
from pathlib import Path

setup_py = Path(sys.argv[1])
llama_src = sys.argv[2]
src = setup_py.read_text()

# ---------------------------------------------------------------------------
# Patch 1: ROOT split
#
# Original (single line):
#   ROOT = Path(__file__).resolve().parent
#
# After patch:
#   ROOT = Path(__file__).resolve().parent
#   _NIX_STORE_ROOT = ROOT                      # immutable: data/, serve/, tools/, …
#   ROOT = Path(os.environ["STRATA_DATA_DIR"])  # mutable state: configs, engine/, logs
#
# Then bulk-redirect read-only subdir refs to _NIX_STORE_ROOT.
# ---------------------------------------------------------------------------
ROOT_LINE = 'ROOT = Path(__file__).resolve().parent\n'
assert ROOT_LINE in src, "ROOT definition line not found in setup.py"

ROOT_PATCH = (
    'ROOT = Path(__file__).resolve().parent\n'
    '_NIX_STORE_ROOT = ROOT                       '
    '# immutable Nix store: data/, serve/, tools/, …\n'
    'ROOT = Path(os.environ["STRATA_DATA_DIR"])   '
    '# mutable state: engine/, configs, logs → user data dir\n'
)
src = src.replace(ROOT_LINE, ROOT_PATCH, 1)

# Bulk-redirect read-only subdir references from ROOT to _NIX_STORE_ROOT.
# These subdirs are never written by setup.py on Linux (sycl is Windows-only,
# tools/hip is AMD-only reads).  The replacement is a simple string substitution
# on the quoted path fragment so it is insensitive to surrounding code changes.
for subdir in (
    '"data"',
    '"serve"',
    '"tools"',
    '"sycl"',
    '"CMakeLists.txt"',
    '"strata.py"',
    '"requirements.txt"',
    '"requirements-build.txt"',
    '"requirements-monitor.txt"',
    '"requirements-nvfp4-local.txt"',
):
    src = src.replace(f'ROOT / {subdir}', f'_NIX_STORE_ROOT / {subdir}')

# ---------------------------------------------------------------------------
# Patch 2: get_llama_cpp() -> return Nix store path directly
#
# Anchor: function signature + final return line (stable; body may change).
# ---------------------------------------------------------------------------
m = re.search(
    r"^def get_llama_cpp\(\):.*?^    return llama\n",
    src, re.MULTILINE | re.DOTALL
)
assert m, "get_llama_cpp() not found in setup.py"
src = src.replace(m.group(0), (
    "def get_llama_cpp():\n"
    "    \"\"\"Nix: return pre-fetched llama.cpp from the Nix store (no download needed).\"\"\"\n"
    "    return Path(" + repr(llama_src) + ")\n"
))

# ---------------------------------------------------------------------------
# Patch 3: pip_install() -> no-op
#
# Anchor: function signature + final "installed" line (stable).
# ---------------------------------------------------------------------------
m = re.search(
    r'^def pip_install\(packages, what(?:, extra=\(\))?\):.*?^    ok\(f"\{what\} installed"\)\n',
    src, re.MULTILINE | re.DOTALL
)
assert m, "pip_install() not found in setup.py"
src = src.replace(m.group(0), (
    "def pip_install(packages, what, extra=()):\n"
    "    \"\"\"Nix: all packages are pre-installed in the pythonEnv; pip is not used.\"\"\"\n"
    "    ok(f\"{what} already installed\")\n"
))

# ---------------------------------------------------------------------------
# Patch 4: build_engine_hip() -> use the engine the Nix package built
#
# The engine package writes engine/BUILD.json with source "nix".  Return that
# folder before any compiler/ROCm discovery, so setup never tries to build.
# ---------------------------------------------------------------------------
OLD_HIP = (
    '    acquisition.require("an engine build")\n'
    '    eng = ROOT / "engine"\n'
    '    eng.mkdir(exist_ok=True)\n'
)
NEW_HIP = (
    '    eng = ROOT / "engine"\n'
    '    try:\n'
    '        _nix_meta = json.loads((eng / "BUILD.json").read_text(encoding="utf-8"))\n'
    '    except (OSError, ValueError):\n'
    '        _nix_meta = {}\n'
    '    if _nix_meta.get("source") == "nix" and (eng / EXE).exists():\n'
    '        return eng\n'
    '    acquisition.require("an engine build")\n'
    '    eng.mkdir(exist_ok=True)\n'
)
assert OLD_HIP in src, "build_engine_hip() anchor not found in setup.py"
src = src.replace(OLD_HIP, NEW_HIP, 1)

# ---------------------------------------------------------------------------
# Patch 5: engine_update_note() -> do not warn about a Nix-managed engine
#
# Anchor: the "need" line unique to engine_update_note().
# ---------------------------------------------------------------------------
OLD_NOTE = (
    '        need = WIN_HIP_MIN_ENGINE if (meta.get("backend") == "hip" and WIN) else MIN_ENGINE\n'
)
NEW_NOTE = (
    '        if meta.get("source") == "nix":\n'
    '            continue\n'
    '        need = WIN_HIP_MIN_ENGINE if (meta.get("backend") == "hip" and WIN) else MIN_ENGINE\n'
)
assert OLD_NOTE in src, "engine_update_note() anchor not found in setup.py"
src = src.replace(OLD_NOTE, NEW_NOTE, 1)

# ---------------------------------------------------------------------------
# Patch 6: update_installed_engine() -> never replace a Nix-managed engine
#
# Anchor: the HIP-on-Windows branch, the first use after reading the metadata.
# ---------------------------------------------------------------------------
OLD_UPDATE = (
    '    meta_text = info.read_text(encoding="utf-8")\n'
    '    meta = json.loads(meta_text)\n'
    '    if meta.get("backend") == "hip" and WIN:           '
)
NEW_UPDATE = (
    '    meta_text = info.read_text(encoding="utf-8")\n'
    '    meta = json.loads(meta_text)\n'
    '    if meta.get("source") == "nix":\n'
    '        return\n'
    '    if meta.get("backend") == "hip" and WIN:           '
)
assert OLD_UPDATE in src, "update_installed_engine() anchor not found in setup.py"
src = src.replace(OLD_UPDATE, NEW_UPDATE, 1)

setup_py.write_text(src)
print("setup.py patched successfully")
