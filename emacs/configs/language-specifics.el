(provide 'language-specifics)

(electric-pair-mode 1)
;; (electric-indent-mode nil)
(global-eldoc-mode -1)

(setq-default indent-tabs-mode 't
              evil-shift-width 8)

(defvaralias 'c-basic-offset 'tab-width)

(defun set-indents (tab-width-p shift-width-p tabs-p)
  (setq-local tab-width tab-width-p
              evil-shift-width shift-width-p
              indent-tabs-mode tabs-p
              java-ts-mode-indent-offset shift-width-p
              csharp-ts-mode-indent-offset shift-width-p))

;; For filetypes without hooks
(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.art\\'" buffer-file-name))
              (set-indents 8 8 t)
              (editorconfig-apply)
              (setq-local artemis-mode 1))
            ))

(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)
        (csharp-mode . csharp-ts-mode)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-indents 16 16 't)
            (olivetti-mode)
            (editorconfig-apply)))

(defun vterm-slime ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when (not (get-buffer "paste-vterm"))
      (split-window-horizontally)
      (windmove-right)
      (multi-vterm)
      (rename-buffer "paste-vterm"))))

(defun send-to-vterm ()
  "Paste the clipboard into the ghci session wrapped in :{ :}"
  (let ((b (current-buffer)))
    (switch-to-buffer "paste-vterm")
    (vterm-yank)
    (vterm-send-return)
    (switch-to-buffer b)))

(defun slime-n ()
  "Copy the current paragraph and send it to ghci"
  (interactive)
  (save-mark-and-excursion
    (copy-paragraph)
    (send-to-vterm)))

(defun slime-buf ()
  "Copy the current selection and send it to ghci"
  (interactive)
  (mark-whole-buffer)
  (kill-ring-save (region-beginning) (region-end))
  (send-to-vterm))

(defun slime-v ()
  "Copy the current selection and send it to ghci"
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (send-to-vterm))

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
  :hook
  (haskell-mode . (lambda ()
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
  :hook (idris-mode . (lambda ()
                        (set-indents 8 2 nil)
                        (setq-local topsy-mode 0)))
  :config
  (setq idris-interpreter-path "idris2"))

(use-package rustic
  :hook
  (rustic-mode . (lambda ()
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
                   (editorconfig-apply)))
  (rustic-popup-mode . 'evil-emacs-state)
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
        lsp-rust-analyzer-proc-macro-enable t
        rustic-rustfmt-args "--edition=2021"
        lsp-eldoc-hook nil
        lsp-enable-symbol-highlighting nil
        lsp-signature-auto-activate nil
        lsp-inlay-hint-enable t
        lsp-inlay-hints-enable t
        ))

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
            (set-indents 8 2 nil)
            (flycheck-mode)
            (company-mode)
            (add-hook 'before-save-hook 'lisp-kbd t)
            (editorconfig-apply)))

(defun americanise ()
  "Ruin spellings of words like centre or colour to work with HTML
  and CSS that have hardcoded the American spellings"
  (interactive)
  (save-excursion
    (replace-string "centre" "center")
    (replace-string "colour" "color")))

(defun lisp-kbd ()
  (interactive)
  (save-excursion
    (replace-string "kdb" "kbd")))

(defun c-cpp-mode-hook-impl ()
  (set-indents 8 8 t)
  (setq-local xref-etags-mode 1
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-enable 't
              electric-indent-mode -1
              lsp-clients-clangd-arguments '("--header-insertion-decorators=0" "--clang-tidy")
              gdb-many-windows-mode 1
              compile-command "make -ksj ")
  (direnv-update-environment)
  (lsp)
  (lsp-inlay-hints-mode)
  (editorconfig-apply))

(add-hook 'c-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c++-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c-ts-mode-hook 'c-cpp-mode-hook-impl)
(add-hook 'c++-ts-mode-hook 'c-cpp-mode-hook-impl)

(add-hook 'xref-etags-mode-hook 'evil-emacs-state)
(add-hook 'xref--xref-buffer-mode-hook 'evil-emacs-state)

(add-hook 'java-ts-mode-hook
          (lambda ()
            (set-indents 8 8 t)
            (editorconfig-apply)
            (lsp-inlay-hints-mode)
            (lsp)))

(add-hook 'csharp-ts-mode-hook
          (lambda ()
            (set-indents 8 8 t)
            (editorconfig-apply)
            (lsp-inlay-hints-mode)
            (setq-local compile-command "time dotnet run"
                        lsp-ui-sideline-enable 't)
            (direnv-update-environment)
            (lsp)))

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
            (set-indents 8 8 't)
            (editorconfig-apply)
            (add-hook 'before-save-hook 'americanise 't)))

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local js-indent-offset 8)
            (set-indents 8 8 't)
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
  :hook
  (nix-mode . (lambda ()
                (set-indents 4 4 t)
                (editorconfig-apply)))
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package olivetti
  :hook
  (olivetti-mode . (lambda () (olivetti-set-width (+ 5 fill-column)))))

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (setq-local vterm-term-environment-variable 'eterm-color
                              vterm-kill-buffer-on-exit t
                              vterm-timer-delay nil)
                  (evil-emacs-state))))

(use-package treemacs)

(use-package jasmin
  :hook
  (jasmin-mode . (lambda () (set-indents 8 8 t)))
  :config
  (setq jasmin-instruction-indent 8
        jasmin-label-indent 0
        jasmin-unknown-line-indent 32
        jasmin-tableswitch-case-indent 16
        jasmin-method-directive-indent 0
        jasmin-global-directive-indent 0))

(use-package futhark-mode
  :hook
  (futhark-mode . (lambda () (set-indents 8 2 nil)))
  :config
  (add-to-list 'lsp-language-id-configuration '(futhark-mode . "futhark"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("futhark" "lsp"))
                    :activation-fn (lsp-activate-on "futhark")
                    :server-id 'futhark)))

(use-package agda2-mode
  :hook
  (agda2-mode . (lambda () (set-indents 8 2 nil))))

(defvaralias 'kotlin-tab-width 'tab-width)
(defvaralias 'kotlin-mode-parenthesized-expression-offset 'tab-width)
(defvaralias 'kotlin-mode-multiline-statement-offset 'tab-width)
(use-package kotlin-mode
  :hook
  (kotlin-mode . (lambda ()
                   (set-indents 8 8 t)
                   (editorconfig-apply)
                   (direnv-update-environment)
                   (setq-local lsp-ui-sideline-show-hover nil
                               eldoc-mode nil)
                   (lsp))))

;; Line numbers
(global-display-line-numbers-mode 't) ;; Needed because reasons

;; Line numbers

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode
               treemacs-mode snake-mode tetris-mode solitaire-mode pong-mode
               image-mode pdf-view-mode)
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
