(provide 'language-checking)

;; ChatGPT's suggestion to disable eglot
(setq eglot-auto-display-help-buffer nil)

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-signature-auto-activate t
        lsp-signature-doc-lines 1
        lsp-signature-render-documentation nil
        read-process-output-max (* 1024 1024) ;; LSP perf hack
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-place-position 'above-line)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point)
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package flycheck)
