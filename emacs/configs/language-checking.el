;; -*- lexical-binding: t -*-

(provide 'language-checking)

;; ChatGPT's suggestion to disable eglot
(setq eglot-auto-display-help-buffer nil)

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 1)
  (lsp-signature-render-documentation nil)
  (read-process-output-max (* 1024 1024)) ;; LSP perf hack
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-place-position 'above-line)
  (lsp-auto-execute-action nil)

  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-treemacs)

(use-package flycheck)
