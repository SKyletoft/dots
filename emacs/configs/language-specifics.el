(provide 'language-specifics)

(electric-pair-mode 1)
;; (electric-indent-mode nil)
(global-eldoc-mode -1)

(setq-default indent-tabs-mode t
              evil-shift-width 8)

(defvaralias 'c-basic-offset 'tab-width)

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
              (setq-local artemis-mode 1))
            ))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-indents 16 16 t)
            (olivetti-mode)
            (editorconfig-apply)))

(defun ghci ()
  "Spawn a ghci terminal in a buffer named ghci. If currently in haskell-mode, load the current file"
  (interactive)
  (let ((file-name (buffer-file-name))
        (is-haskell (eq major-mode 'haskell-mode)))
    (when (not (get-buffer "ghci"))
      (split-window-horizontally)
      (windmove-right)
      (multi-vterm)
      (rename-buffer "ghci")
      (if is-haskell
          (progn
            (vterm-insert "ghci ")
            (vterm-insert file-name)
            (vterm-send-return))
        (progn
          (vterm-insert "ghci")
          (vterm-send-return))))))

(defun copy-paragraph ()
  "Mark and copy the current paragraph"
  (backward-paragraph)
  (let ((start (point)))
    (forward-paragraph)
    (let ((end (point)))
      (kill-ring-save start end))))

(defun send-to-ghci ()
  "Paste the clipboard into the ghci session wrapped in :{ :}"
  (let ((b (current-buffer)))
    (switch-to-buffer "ghci")
    (vterm-insert ":{\n")
    (vterm-yank)
    (vterm-send-return)
    (vterm-insert ":}")
    (vterm-send-return)
    (switch-to-buffer b)))

(defun hs-slime-n ()
  "Copy the current paragraph and send it to ghci"
  (interactive)
  (save-mark-and-excursion
    (copy-paragraph)
    (send-to-ghci)))

(defun hs-slime-v ()
  "Copy the current selection and send it to ghci"
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (send-to-ghci))

(defun hs-slime-dwim ()
  "Do hs-slime-n or hs-slime-v depending on if there's a current selection"
  (interactive)
  (if (region-active-p)
      (hs-slime-v)
    (hs-slime-n)))

(defun hs-run ()
  "Reload the file in ghci and run `main`'"
  (interactive)
  (save-mark-and-excursion
    (let ((b (current-buffer)))
      (switch-to-buffer "ghci")
      (vterm-insert ":r\nmain")
      (vterm-send-return)
      (switch-to-buffer b))))

(use-package haskell-mode
  :config

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
            (editorconfig-apply)
            ;; (ghci)
            )))

(defun toggle-hole ()
  "Toggle having a ? at the start of the word"
  (interactive)
  (save-excursion
    ;; Unless we check this first it will toggle the ? on the previous
    ;; word if both this and the previous word are holes
    (if (eq ?? (char-before))
        (backward-delete-char 1)
      (if (not (eq ?\s (char-before)))
          (backward-word))
      (cond ((eq ?? (char-before)) (backward-delete-char 1))
            ((eq ?? (char-after)) (delete-char 1))
            (t (insert "?"))))))

(use-package idris-mode
  :config
  (setq idris-interpreter-path "idris2")
  (add-hook 'idris-mode-hook
            (lambda ()
              (set-indents 8 2 nil)
              (setq-local topsy-mode 0)
              )))

(use-package rustic
  :config
  (setq rustic-format-on-save nil
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-reborrow-hints nil
        rustic-rustfmt-args "--edition=2021"
        lsp-eldoc-hook nil
        lsp-enable-symbol-highlighting nil
        lsp-signature-auto-activate nil
        lsp-inlay-hint-enable t
        lsp-inlay-hints-enable t
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
                          lsp-ui-sideline-enable t
                          fill-column 100)
              ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
              (lsp-lens-hide)
              (lsp-inlay-hints-mode)
              (editorconfig-apply)
              ))
  (add-hook 'rustic-popup-mode-hook 'evil-emacs-state))

(defun rust-compile-and-dap ()
  (interactive)
  (let* ((workspace (lsp-workspace-root))
         (project-name (file-name-nondirectory workspace))
         (exe-dir (concat workspace
                          "/target/debug/"
                          project-name)))
    (save-window-excursion (rustic-cargo-build))
    (dap-debug (list :type "gdb"
                     :request "launch"
                     :name "GDB::Run Cargo project"
                     :gdbpath "rust-gdb"
                     :target exe-dir
                     :cwd workspace))
    (save-window-excursion
      (switch-buffer "*GDB::Run Cargo project out*")
      (evil-motion-state)
      (switch-buffer "*GDB::Run Cargo project stderr*")
      (evil-motion-state))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-elsa-setup)
            (setq-local flycheck-mode 1
                        indent-tabs-mode nil)
            (evil-define-key 'normal emacs-lisp-mode-map (kbd "SPC i") 'indent-according-to-mode)
            (evil-define-key 'visual emacs-lisp-mode-map (kbd "SPC i") 'indent-region)
            (editorconfig-apply)))

(defun americanise ()
  "Ruin spellings of words like centre or colour to work with HTML
  and CSS that have hardcoded the American spellings"
  (interactive)
  (save-excursion
    (replace-string "centre" "center")
    (replace-string "colour" "color")))

(defun c-cpp-mode-hook-impl ()
  (set-indents 8 8 t)
  (setq-local xref-etags-mode 1
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-enable t
              electric-indent-mode -1
              lsp-clients-clangd-arguments "--clang-tidy"
              )
  (lsp)
  (editorconfig-apply))

(add-hook 'c-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c++-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c-ts-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c++-ts-mode-hook 'c-cpp-mode-hook-impl)

(add-hook 'xref-etags-mode-hook 'evil-emacs-state)
(add-hook 'xref--xref-buffer-mode-hook 'evil-emacs-state)

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
            (lsp)
            (editorconfig-apply)))

(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))

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

(use-package olivetti
  :config
  (add-hook 'olivetti-mode-hook
            (lambda ()
              (olivetti-set-width (+ 5 fill-column)))))

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
              (evil-emacs-state))))

(use-package treemacs)

(use-package jasmin
  :config
  (setq jasmin-instruction-indent 8
        jasmin-label-indent 0
        jasmin-unknown-line-indent 32
        jasmin-tableswitch-case-indent 16
        jasmin-method-directive-indent 0
        jasmin-global-directive-indent 0)
  (add-hook 'jasmin-mode-hook
            (lambda ()
              (set-indents 8 8 t))))

(use-package futhark-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(futhark-mode . "futhark"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("futhark" "lsp"))
                    :activation-fn (lsp-activate-on "futhark")
                    :server-id 'futhark))
  (add-hook 'futhark-mode-hook
            (lambda ()
              (set-indents (8 2 nil)))))

(use-package agda2-mode
  :config
  (add-hook 'agda2-mode-hook
            (lambda ()
              (set-indents (8 2 nil)))))

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
