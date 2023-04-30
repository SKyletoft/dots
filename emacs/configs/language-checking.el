(provide 'language-checking)

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-signature-auto-activate t
        lsp-signature-doc-lines 1
        lsp-signature-render-documentation nil
        read-process-output-max (* 1024 1024)) ;; LSP perf hack
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package flycheck)
