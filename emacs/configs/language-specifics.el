(provide 'language-specifics)

(electric-indent-mode nil)
(global-eldoc-mode -1)

(setq-default indent-tabs-mode t
              evil-shift-width 8)

(defun set-indents (tab-width-p shift-width-p tabs-p)
  (setq-local tab-width tab-width-p
              evil-shift-width shift-width-p
              indent-tabs-mode tabs-p))

;; For filetypes without hooks
(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.art\\'" buffer-file-name))
              (set-indents 8 8 t)
              (editorconfig-apply)
              (setq-local prog-mode 1))
            ))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-indents 16 16 t)
            (olivetti-mode)
            (olivetti-set-width 70)
            (editorconfig-apply)))

(defun hs-slime ()
  (interactive)
  (save-excursion
    (let ((b (current-buffer)))
      (backward-paragraph)
      (let ((start (point)))
        (forward-paragraph)
        (let ((end (point)))
          (kill-ring-save start end)))
      (switch-to-buffer "*vterminal<1>*")
      (vterm-insert ":{")
      (vterm-yank)
      (vterm-insert ":}")
      (vterm-send-return)
      (switch-to-buffer b)
      )))

(add-hook 'haskell-mode-hook
          (lambda ()
            (lsp)
            (set-indents 8 2 nil)
            (setq-local lsp-eldoc-enable-hover nil
                        eldoc-documentation-function #'ignore
                        eldoc-mode nil
                        lsp-haskell-plugin-ghcide-type-lenses-global-on nil
                        lsp-haskell-plugin-ghcide-class-global-on nil)
            (lsp-restart-workspace)
            (lsp-ui-doc-mode t)
            (define-key evil-normal-state-map (kbd "SPC g") 'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "SPC a") 'lsp-execute-code-action)
            (define-key evil-normal-state-map (kbd "SPC f") 'lsp-ui-doc-glance)
            (define-key evil-normal-state-map (kbd "SPC i") ":!hindent % && stylish-haskell -i %<CR>")
            (define-key evil-normal-state-map (kbd "SPC b") 'hs-slime)
            (editorconfig-apply)
            ))

(use-package rustic
  :config
  (setq rustic-format-on-save t
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-reborrow-hints t
        rustic-format-on-save nil
        ;; rustic-rustfmt-args "+nightly"
        lsp-eldoc-hook nil
        lsp-enable-symbol-highlighting nil
        lsp-signature-auto-activate nil
        )
  (add-hook 'rustic-mode-hook
            (lambda ()
              (when buffer-file-name
                (setq-local buffer-save-without-query t))
              (set-indents 8 8 t)
              (setq-local rust-indent-offset 8
                          lsp-idle-delay 0.6
                          eldoc-mode nil
                          lsp-lens-mode nil
                          lsp-eldoc-enable-hover nil
                          eldoc-documentation-function #'ignore
                          lsp-ui-doc-mode t
                          lsp-ui-sideline-show-hover nil
                          lsp-ui-sideline-enable t)

              (define-key evil-normal-state-map (kbd "SPC i") 'rustic-format-buffer)
              (define-key evil-normal-state-map (kbd "SPC f") 'lsp-ui-doc-glance)
              (define-key evil-normal-state-map (kbd "SPC g") 'xref-find-definitions)
              (define-key evil-normal-state-map (kbd "SPC a") 'lsp-execute-code-action)
              (define-key evil-normal-state-map (kbd "SPC t") 'lsp-rust-analyzer-inlay-hints-mode)
              (define-key evil-normal-state-map (kbd "<f2>") 'lsp-rename)

              ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
              (editorconfig-apply)
              )))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-elsa-setup)
            (setq-local flycheck-mode 1
                        indent-tabs-mode nil)
            (define-key evil-normal-state-map (kbd "SPC i") 'indent-according-to-mode)
            (define-key evil-visual-state-map (kbd "SPC i") 'indent-region)
            (editorconfig-apply)))

(defun americanise ()
  "Ruin spellings of words like centre or colour to work with HTML
  and CSS that have hardcoded the American spellings"
  (interactive)
  (let ((pos (point))) ;; Save cursor pos

    (replace-string "centre" "center")
    (replace-string "colour" "color")

    (goto-char pos))) ;; Restore cursor pos

(defun c-cpp-mode-hook-impl ()
  (set-indents 8 8 t)
  (setq-local xref-etags-mode 1
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-enable t
              electric-indent-mode -1
              )
  (lsp)
  (define-key evil-normal-state-map (kbd "SPC f") 'lsp-ui-doc-glance)
  (define-key evil-normal-state-map (kbd "SPC g") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "SPC a") 'lsp-execute-code-action)
  (editorconfig-apply))
  

(add-hook 'c-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c++-mode-hook 'c-cpp-mode-hook-impl)

(add-hook 'xref-etags-mode-hook 'evil-emacs-state)

(add-hook 'java-mode-hook
          (lambda ()
            (set-indents 8 8 t)
            (editorconfig-apply)))

(add-hook 'erlang-mode-hook
          (lambda ()
            (set-indents 3 3 t)
            (editorconfig-apply)))

(add-hook 'mhtml-mode-hook
          (lambda ()
            (setq-local html-indent-offset 8
                        electric-indent-mode nil)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (add-hook 'before-save-hook 'americanise t)))

(add-hook 'css-mode-hook
          (lambda ()
            (setq-local css-indent-offset 8)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (add-hook 'before-save-hook 'americanise t)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local js-indent-offset 8)
            (set-indents 8 8 t)
            (editorconfig-apply)))

(add-hook 'tuareg-mode-hook
          (lambda ()
            (ocamlformat-setup-indents)
            (set-indents 8 2 nil)
            (setq-local eldoc-mode nil)
            (define-key evil-normal-state-map (kbd "SPC g") 'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "SPC a") 'lsp-execute-code-action)
            (define-key evil-normal-state-map (kbd "SPC f") 'lsp-ui-doc-glance)
            (define-key evil-normal-state-map (kbd "SPC i") 'ocamlformat)
            (lsp)
            (editorconfig-apply)))

(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook
            (lambda ()
              (set-indents 4 4 t)
              (editorconfig-apply)
              ))
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local vterm-term-environment-variable 'eterm-color
                          vterm-kill-buffer-on-exit t
                          vterm-timer-delay nil)
              (evil-emacs-state)))
  (define-key vterm-mode-map (kbd "C-M-w") 'windmove-up)
  (define-key vterm-mode-map (kbd "C-M-s") 'windmove-down)
  (define-key vterm-mode-map (kbd "C-M-a") 'windmove-left)
  (define-key vterm-mode-map (kbd "C-M-d") 'windmove-right))

;; Line numbers
(global-display-line-numbers-mode t) ;; Needed because reasons

;; Line numbers

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode
               treemacs-mode snake-mode tetris-mode solitaire-mode pong-mode)
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
