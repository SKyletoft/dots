;; -*- lexical-binding: t -*-

(setenv "LSP_USE_PLISTS" "true") ; LSP-perf hack, a part of lsp-booster

(setq load-prefer-newer t
      native-comp-jit-compilation t
      native-comp-deferred-compilation native-comp-jit-compilation)
