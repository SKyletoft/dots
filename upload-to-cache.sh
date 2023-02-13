#!/usr/bin/env sh

set -eu
set -f # disable globbing
export IFS=' '

echo "Signing and uploading paths" $OUT_PATHS
exec nix copy --to 'https://nix.u3836.se?secret-key=/run/keys/nix-signing-key' $OUT_PATHS
