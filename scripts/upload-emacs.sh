#!/usr/bin/env bash

KEY=~/.secret/eurydice-private-key
EMACS=$(ls -l $(which emacs) | awk '{print $11;}' | sed 's|/bin/emacs||')

doas nix store sign --key-file $KEY --recursive $EMACS
nix store verify --trusted-public-keys $(nix key convert-secret-to-public < $KEY) $EMACS
NIX_SSHOPTS="-p1234" nix copy --to ssh://nix.u3836.se $EMACS
