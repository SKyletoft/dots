(provide 'language-specifics)

(electric-indent-mode nil)
(global-eldoc-mode -1)

(setq-default indent-tabs-mode t)
(setq-default evil-shift-width 8)

(defun set-indents (tab-width-p shift-width-p tabs-p)
  (setq-local tab-width tab-width-p)
  (setq-local evil-shift-width shift-width-p)
  (setq-local indent-tabs-mode tabs-p))

;; For filetypes without hooks
(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.art\\'" buffer-file-name))
              (set-indents 8 8 t)
              (setq-local prog-mode 1))
            (when (and (stringp buffer-file-name)
                       (string-match "\\.nix\\'" buffer-file-name))
              (set-indents 4 4 t))
            ))

(defun haskell-post-lsp ()
  (interactive)
  (lsp)
  (lsp-ui-doc-mode t))

(add-hook 'haskell-mode-hook
          (lambda ()
            (set-indents 8 2 nil)
            (setq-local lsp-eldoc-enable-hover nil)
            (setq-local eldoc-documentation-function #'ignore)
            (setq-local eldoc-mode nil)
            ;; Needs to run after direnv
            ;; (haskell-post-lsp)
            (define-key evil-normal-state-map (kbd "SPC g") 'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "SPC a") 'lsp-execute-code-action)
            (define-key evil-normal-state-map (kbd "SPC f") 'lsp-ui-doc-glance)
            (define-key evil-normal-state-map (kbd "SPC i") ":!hindent % && stylish-haskell -i %<CR>")))

(use-package rustic
  :config
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (when buffer-file-name
                (setq-local buffer-save-without-query t))
              (setq-local rust-indent-offset 8)
              (set-indents 8 8 t)

              (setq-local lsp-rust-analyzer-cargo-watch-command "clippy")
              (setq-local lsp-idle-delay 0.6)
              (setq-local lsp-rust-analyzer-server-display-inlay-hints t)
              (setq-local lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
              (setq-local lsp-rust-analyzer-display-chaining-hints t)
              (setq-local lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
              (setq-local lsp-rust-analyzer-display-closure-return-type-hints t)
              (setq-local lsp-rust-analyzer-display-parameter-hints t)
              (setq-local lsp-rust-analyzer-display-reborrow-hints t)
              (setq-local rustic-format-on-save nil)
              (setq-local rustic-rustfmt-args "+nightly")

              (setq-local eldoc-mode nil)
              (setq-local lsp-eldoc-enable-hover nil)
              (setq-local eldoc-documentation-function #'ignore)
              (setq-local lsp-ui-doc-mode t)
              (setq-local lsp-ui-sideline-show-hover nil)
              (setq-local lsp-ui-sideline-enable nil)

              (define-key evil-normal-state-map (kbd "SPC i") 'rustic-format-buffer)
              (define-key evil-normal-state-map (kbd "SPC f") 'lsp-ui-doc-glance)
              (define-key evil-normal-state-map (kbd "SPC g") 'xref-find-definitions)
              (define-key evil-normal-state-map (kbd "SPC a") 'lsp-execute-code-action)
              (define-key evil-normal-state-map (kbd "SPC t") 'lsp-rust-analyzer-inlay-hints-mode)

              ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
              )))

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-doc-lines 1) 
  (setq lsp-signature-render-documentation nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package flycheck)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local indent-tabs-mode nil)
            (define-key evil-normal-state-map (kbd "SPC i") 'indent-according-to-mode)
            (define-key evil-visual-state-map (kbd "SPC i") 'indent-region)))

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local vterm-term-environment-variable 'eterm-color)
              (setq-local vterm-kill-buffer-on-exit t)
              (setq-local vterm-timer-delay nil)
              (evil-emacs-state)))
  (define-key vterm-mode-map (kbd "C-M-w") 'windmove-up)
  (define-key vterm-mode-map (kbd "C-M-s") 'windmove-down)
  (define-key vterm-mode-map (kbd "C-M-a") 'windmove-left)
  (define-key vterm-mode-map (kbd "C-M-d") 'windmove-right))

;; Line numbers
(global-display-line-numbers-mode t) ;; Needed because reasons

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode treemacs-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)
    (setq-local display-line-numbers 'relative)))

(global-display-line-numbers-mode)
