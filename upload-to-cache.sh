# set -eu
# set -f # disable globbing
# export IFS=' '

# echo "Signing and uploading paths" $OUT_PATHS
# exec nix copy --to 'https://nix.u3836.se?secret-key=/run/keys/nix-signing-key' $OUT_PATHS
# exec nix copy --to 'https://nix.u3836.se' $OUT_PATHS

set -eu

export KEY="$(cat /home/u3836/.ssh/nix.u3836.se)"

if [ -f "$KEY" ]; then
  if [ -n "$OUT_PATHS" ]; then
    # send copy operations to a task queue so the next build can start
    nix run nixos#ts -- nix copy --to "$STORE" "$OUT_PATHS" || {
      echo "No 'nixos' registry pin..."
      exit 0
    }
  else
    # this can happen if we are just using `nix build --rebuild` to check a package
    echo "Nothing to upload"
  fi

else
  echo "No signing key"
fi
