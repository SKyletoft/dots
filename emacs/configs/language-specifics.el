;;; language-specifics.el --- Configuration for specific languages  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(electric-pair-mode 1)
;; (electric-indent-mode nil)
(global-eldoc-mode -1)

(setq-default indent-tabs-mode 't
              evil-shift-width 8)

;; Evil shift width is the only one I control in my bindings
;; (tab/S-tab) so just alias all the language specific ones to that
(defvaralias 'c-basic-offset                              'evil-shift-width)
(defvaralias 'java-ts-mode-indent-offset                  'evil-shift-width)
(defvaralias 'c-ts-mode-indent-offset                     'evil-shift-width)
(defvaralias 'c++-ts-mode-indent-offset                   'evil-shift-width)
(defvaralias 'rust-ts-mode-indent-offset                  'evil-shift-width)
(defvaralias 'rust-indent-offset                          'evil-shift-width)
(defvaralias 'csharp-ts-mode-indent-offset                'evil-shift-width)
(defvaralias 'nix-ts-mode-indent-offset                   'evil-shift-width)
(defvaralias 'css-indent-offset                           'evil-shift-width)
(defvaralias 'js-indent-offset                            'evil-shift-width)
(defvaralias 'js-indent-level                             'evil-shift-width)
(defvaralias 'html-indent-offset                          'evil-shift-width)
(defvaralias 'kotlin-tab-width                            'evil-shift-width)
(defvaralias 'kotlin-mode-parenthesized-expression-offset 'evil-shift-width)
(defvaralias 'kotlin-mode-multiline-statement-offset      'evil-shift-width)
(defvaralias 'sh-indentation                              'evil-shift-width)
(defvaralias 'python-indent-offset                        'evil-shift-width)
(defvaralias 'html-ts-mode-indent-offset                  'evil-shift-width)
(defvaralias 'swift-ts-mode-indent-offset                 'evil-shift-width)
(defvaralias 'typst-ts-mode-indent-offset                 'evil-shift-width)
(defvaralias 'kdl-ts-mode-indent-offset                   'evil-shift-width)
(defvaralias 'js-indent-level                             'evil-shift-width)
(defvaralias 'typescript-ts-mode-indent-offset            'evil-shift-width)
(defvaralias 'json-ts-mode-indent-offset                  'evil-shift-width)
(defvaralias 'go-ts-mode-indent-offset                    'evil-shift-width)
(defvaralias 'typst-ts-indent-offset                      'evil-shift-width)

(defadvice align-regexp (around smart-tabs activate)
  "Disables tab characters in alignment."
  (let ((indent-tabs-mode nil)) ad-do-it))

(defun set-indents (tab-width-p shift-width-p tabs-p)
  "Set the tab width (TAB-WIDTH-P), evil shift width (SHIFT-WIDTH-P) and whether or not to use tabs (TABS-P)."
  (setq-local tab-width tab-width-p
              evil-shift-width shift-width-p
              indent-tabs-mode tabs-p))

(defun try-direnv-update-directory-environment ()
  (interactive)
  (if (not (file-remote-p default-directory))
      (direnv-update-directory-environment)))

;; For filetypes without hooks
(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "bashrc" buffer-file-name))
              (bash-ts-mode))
            (when (and (stringp buffer-file-name)
                       (string-match "\\.go\\'" buffer-file-name))
              (go-ts-mode))
            (when (and (stringp buffer-file-name)
                       (string-match "\\.art\\'" buffer-file-name))
              (set-indents 8 8 t)
              (editorconfig-apply)
              (setq-local artemis-mode 1))
            (when (and (stringp buffer-file-name)
                       (string-match "\\.agda\\'" buffer-file-name))
              (try-direnv-update-directory-environment)
              (load-file (let ((coding-system-for-read 'utf-8))
                           (shell-command-to-string "agda-mode locate")))
              (agda2-mode)
              (add-hook 'before-save-hook 'agda-arrow-fix 90 't)
              (set-indents 8 2 nil))))

(setq major-mode-remap-alist
      '((java-mode . java-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-mode . c-ts-mode)
        (javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (js-json-mode . json-ts-mode)
        ;; (html-mode . html-ts-mode) ; Disabled as tree sitter segfaults right now
        ;; (mhtml-mode . html-ts-mode)
        (nix-mode . nix-ts-mode)
        (rustic-mode . rust-ts-mode)
        (python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (haskell-mode . haskell-ts-mode)
        (csharp-mode . csharp-ts-mode)
        (tex-mode . latex-mode)))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-indents 16 16 't)
            ;; (olivetti-mode)
            (editorconfig-apply)
            (setq-local compile-command (concat "mdpdf "
                                                (buffer-file-name)
                                                " "
                                                (file-name-sans-extension (buffer-file-name))
                                                ".pdf"))))

(defun haskell-hook-fn ()
  (try-direnv-update-environment)
  (set-indents 8 2 nil)
  (setq-local lsp-idle-delay 0.6
              eldoc-mode 0
              lsp-lens-mode 0
              lsp-eldoc-enable-hover nil
              eldoc-documentation-function #'ignore
              lsp-ui-doc-mode t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-enable t
              lsp-haskell-plugin-ghcide-type-lenses-global-on t
              lsp-haskell-plugin-ghcide-class-global-on nil
              compile-command (concat "runhaskell " (buffer-file-name))
              lsp-haskell-formatting-provider "fourmolu"
              lsp-haskell-plugin-fourmolu-config-external t
              lsp-haskell-plugin-fourmolu-config-path "fourmolu18")
  (editorconfig-apply)
  (hs-minor-mode 1)
  (lsp)
  ;; (ghci)
  )

(defun not-eq-fix ()
"Swap out != with /= for languages like Haskell."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "!=" "/=")))

(use-package haskell-mode
  :defer t
  :hook
  (haskell-mode . not-eq-fix)
  (haskell-mode . haskell-hook-fn))
(use-package haskell-ts-mode
  :defer t
  :mode ("\\.hs\\'" . haskell-ts-mode)
  :hook
  (haskell-ts-mode . not-eq-fix)
  (haskell-ts-mode . haskell-hook-fn))

(defun toggle-hole ()
"Toggle having a ? at the start of the word."
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
  :defer t
  :hook (idris-mode . (lambda ()
                        (set-indents 8 2 nil)
                        (setq-local topsy-mode 0)))
  :custom
  (idris-interpreter-path "idris2"))

(defun unwarp ()
"Fix rust's most common typo"
  (interactive)
  (save-excursion
    (goto-line 0)
    (replace-string "unwarp" "unwrap")))

(use-package rust-ts-mode
  :defer nil ; Everything else should defer, but I'm almost always
             ; using Rust and have managed to open a file before it
             ; loads too many times at this point.
  :hook
  (rust-ts-mode . (lambda ()
                    (add-hook 'before-save-hook 'unwarp 90 't)
                    (when buffer-file-name
                      (setq-local buffer-save-without-query t))
                    (set-indents 8 8 t)
                    (setq-local lsp-idle-delay 0.6
                                eldoc-mode nil
                                lsp-lens-mode nil
                                lsp-eldoc-enable-hover nil
                                eldoc-documentation-function #'ignore
                                lsp-ui-doc-mode t
                                lsp-ui-sideline-show-hover nil
                                lsp-ui-sideline-enable t
                                fill-column 100
                                compile-command "cargo --color always run ")
                    (try-direnv-update-environment)
                    (lsp)
                    (lsp-lens-hide)
                    (lsp-inlay-hints-mode)
                    (hs-minor-mode)
                    (editorconfig-apply)))
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-rustfmt-args "--edition=2021")
  (lsp-eldoc-hook nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil)
  (lsp-inlay-hint-enable t)
  (lsp-inlay-hints-enable t))

;; For rustic mode only
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
            (add-hook 'before-save-hook 'lisp-kbd 90 't)
            (prettify-symbols-mode 1)
            (hs-minor-mode 1)
            (editorconfig-apply)))

(defun americanise ()
"Ruin spellings of words like centre or colour to work with HTML and CSS that have hardcoded the American spellings."
  (interactive)
  (save-excursion
    (goto-line 0)
    (replace-string "centre" "center")
    (replace-string "colour" "color")))

(defun lisp-kbd ()
  (interactive)
  (save-excursion
    (goto-line 0)
    (replace-string "kbd" "kbd")))

(defun agda-arrow-fix ()
  (interactive)
  (save-excursion
    (goto-line 0)
    (replace-regexp "\\b->\\b" "→")
    (goto-line 0)
    (replace-regexp "\\b<-\\b" "←")))

(defun c-cpp-mode-hook-impl ()
  (set-indents 8 8 t)
  (setq-local xref-etags-mode 1
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-enable 't
              electric-indent-mode -1
              lsp-clients-clangd-arguments '("--header-insertion-decorators=0" "--clang-tidy")
              gdb-many-windows-mode 1
              compile-command "make -ksj ")
  (hs-minor-mode 1)
  (try-direnv-update-environment)
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
            (hs-minor-mode 1)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (try-direnv-update-environment)
            (setq-local lsp-java-progress-reports-enabled nil)
            (lsp-inlay-hints-mode)
            (lsp)))

(add-hook 'csharp-ts-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (lsp-inlay-hints-mode)
            (setq-local compile-command "time dotnet run"
                        lsp-ui-sideline-enable 't)
            (try-direnv-update-environment)
            (lsp)))

(add-hook 'erlang-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (editorconfig-apply)
            (set-indents 8 2 nil)
            (try-direnv-update-directory-environment)
            (lsp)))

(add-hook 'mhtml-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (setq-local electric-indent-mode nil)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (add-hook 'before-save-hook 'americanise 90 't)))
(add-hook 'html-ts-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (setq-local electric-indent-mode nil)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (add-hook 'before-save-hook 'americanise 90 't)))

(add-hook 'css-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 8 't)
            (editorconfig-apply)
            (add-hook 'before-save-hook 'americanise 90 't)))

(add-hook 'js-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 8 't)
            (editorconfig-apply)))

(add-hook 'tuareg-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 2 nil)
            (setq-local eldoc-mode nil)
            (try-direnv-update-environment)
            (lsp)
            (editorconfig-apply)))

(use-package nix-ts-mode
  :defer t
  :mode "\\.nix\\'"
  :hook
  (nix-ts-mode . (lambda ()
                   (hs-minor-mode 1)
                   (set-indents 4 4 t)
                   (setq-local compile-command "nix run -L")
                   (editorconfig-apply)
                   (try-direnv-update-environment)
                   (lsp))))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 4 nil)
            (setq-local compile-command (concat "python3 "
                                                (buffer-file-name))
                        lsp-pyright-typechecking-mode "standard")
            (try-direnv-update-directory-environment)
            (lsp)))

(add-hook 'bash-ts-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 8 t)
            (editorconfig-apply)
            (try-direnv-update-environment)
            (lsp)))

(add-hook 'latex-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 8 t)
            (editorconfig-apply)))

(use-package pdf-tools
  :defer t
  :hook (pdf-view-mode . (lambda () (auto-revert-mode)))
  :init
  (pdf-tools-install))

(use-package treemacs
  :defer t)

(use-package jasmin
  :load-path "~/dots/emacs/third-party"
  :custom
  (jasmin-instruction-indent 8)
  (jasmin-label-indent 0)
  (jasmin-unknown-line-indent 32)
  (jasmin-tableswitch-case-indent 16)
  (jasmin-method-directive-indent 0)
  (jasmin-global-directive-indent 0)
  :hook
  (jasmin-mode . (lambda () (set-indents 8 8 t))))

(use-package futhark-mode
  :defer t
  :hook
  (futhark-mode . (lambda ()
                    (hs-minor-mode 1)
                    (set-indents 8 2 nil)
                    (try-direnv-update-environment)
                    (lsp)
                    (editorconfig-apply))))

(use-package dafny-2-mode
  :hook
  (dafny-2-mode . (lambda ()
                  (set-indents 8 8 t)
                  (try-direnv-update-environment)
                  (lsp)
                  (lsp-ui-mode 1))))

(require 'lsp)
(add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "Typst"))
(lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                                      :activation-fn (lsp-activate-on "Typst")
                                      :major-modes '(typst-ts-mode)
                                      :server-id 'tinymist))
(require 'typst-ts-mode)
(add-hook 'typst-ts-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (set-indents 8 8 t)
            (setq-local compile-command (concat "typst w "
                                                (buffer-file-name)))
            (try-direnv-update-environment)
            (editorconfig-apply)
            (lsp)))

(use-package wgsl-mode
  :defer t
  :hook
  (wgsl-mode . (lambda ()
                 (hs-minor-mode 1)
                 (set-indents 8 8 t)
                 (try-direnv-update-environment)
                 (lsp)))
  :config
  (setq-local compile-command "cargo run"
              wgsl-inlay-hints-enabled t
              wgsl-inlay-hints-types t
              wgsl-inlay-hints-parameters t
              wgsl-inlay-hints-struct-layout nil
              wgsl-inlay-hints-type-verbosity "compact"
              wgsl-diagnostics-types t
              wgsl-diagnostics-naga-parsing nil
              wgsl-diagnostics-naga-validation t
              wgsl-diagnostics-naga-version "0.14"))

(defun glsl-hook ()
  (interactive)
  (hs-minor-mode 1)
  (define-key glsl-mode-map (kbd "S-<iso-lefttab>") 'ff-find-other-file 'remove)
  (set-indents 8 8 t)
  (editorconfig-apply)
  (try-direnv-update-environment)
  (lsp))

(use-package glsl-mode
  :hook
  (glsl-mode . glsl-hook)
  (glsl-ts-mode . glsl-hook))

(use-package pest-mode
  :defer t
  :hook
  (pest-mode . (lambda ()
                 (hs-minor-mode 1)
                 (set-indents 8 8 t)))
  :config
  (autoload 'pest-mode "pest-mode")
  (add-to-list #'auto-mode-alist '("\\.pest\\'" . pest-mode)))

(use-package go-ts-mode
  :defer t
  :hook
  (go-ts-mode . (lambda ()
                  (hs-minor-mode 1)
                  (set-indents 8 8 t)
                  (editorconfig-apply)
                  (try-direnv-update-environment)
                  (setq-local compile-command "go run .")
                  (lsp))))

(use-package kotlin-mode
  :defer t
  :hook
  (kotlin-mode . (lambda ()
                   (hs-minor-mode 1)
                   (set-indents 8 8 t)
                   (editorconfig-apply)
                   (try-direnv-update-environment)
                   (setq-local lsp-ui-sideline-show-hover nil
                               eldoc-mode nil
                               compile-command "make -skj -C ../../.. ")
                   (lsp))))

(use-package typescript-ts-mode
  :defer t
  :custom
  (lsp-javascript-display-parameter-name-hints 'all)
  (lsp-javascript-display-parameter-name-hints-when-argument-matches-name nil)
  (lsp-javascript-display-variable-type-hints t)
  (lsp-javascript-display-return-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  :hook
  (typescript-ts-mode . (lambda ()
                          (try-direnv-update-directory-environment)
                          (hs-minor-mode 1)
                          (eldoc-mode -1)
                          (editorconfig-apply)
                          (setq-local lsp-ui-sideline-show-symbol nil)
                          (lsp))))

(use-package roc-ts-mode
  :defer t
  :hook
  (roc-ts-mode . (lambda ()
                   (hs-minor-mode 1)
                   (set-indents 8 4 nil)
                   (editorconfig-apply)
                   (setq-local compile-command "roc run")))
  :config
  (with-eval-after-load 'roc-ts-mode
    (require 'lsp-mode)
    (add-to-list 'lsp-language-id-configuration '(roc-ts-mode . "roc"))
    (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection "roc_language_server")
                                          :activation-fn (lsp-activate-on "roc")
                                          :major-modes '(roc-ts-mode)
                                          :server-id 'roc_language_server))
    (add-hook 'roc-ts-mode-hook #'lsp-deferred)))

(use-package swift-ts-mode
  :hook
  (swift-ts-mode . (lambda ()
                     (hs-minor-mode 1)
                     (set-indents 8 8 t)
                     (editorconfig-apply)
                     (setq-local compile-command "swift run"))))
;; (use-package lsp-sourcekit)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "sourcekit-lsp")
                  :activation-fn (lsp-activate-on "swift-ts")
                  :major-modes '(swift-mode swift-ts-mode)
                  :server-id 'sourcekit-ls))
(add-to-list 'lsp-language-id-configuration '(swift-ts-mode . "swift-ts"))

(use-package yaml-ts-mode
  :hook
  (yaml-ts-mode . (lambda ()
                    (hs-minor-mode 1)
                    (set-indents 8 2 nil)
                    (editorconfig-apply)
                    (mixed-pitch-mode -1))))

(use-package kdl-ts-mode
  :load-path "~/dots/emacs/third-party/kdl-ts-mode"
  :hook
  (kdl-ts-mode . (lambda ()
                   (hs-minor-mode 1)
                   (editorconfig-apply))))

(use-package qml-mode)

(use-package ponylang-mode
  :hook (ponylang-mode . (lambda ()
                           (set-indents 2 2 nil)
                           (editorconfig-apply))))

(defvar-keymap apl-keymap
  "§" "⋄"
  "|" "⋄"
  "¦" "⌺"
  "½" "⌺"
  "1" "¨"
  "!" "⌶"
  "2" "¯"
  "\"" "⍫"
  ;; "3" "<"
  "#" "⍒"
  "4" "≤"
  "$" "⍋"
  ;; "5" "="
  "%" "⌽"
  "6" "≥"
  "&" "⍉"
  ;; "7" ">"
  "/" "⊖"
  "8" "≠"
  "(" "⍟"
  "9" "∨"
  ")" "⍱"
  "0" "∧"
  "=" "⍲"
  "+" "×"
  ;; "?" "!"
  "´" "÷"
  "`" "⌹"
  ;; "q" "?"
  "Q" "⍰"
  "w" "⍵"
  "W" "⍹"
  "e" "∊"
  "E" "⍷"
  "r" "⍴"
  "R" "⌾"
  "t" "~"
  "T" "⍨"
  "y" "↑"
  ;; "Y" "↑"
  "u" "↓"
  ;; "U" "↓"
  "i" "⍳"
  "I" "⍸"
  "o" "○"
  "O" "⍥"
  "p" "*"
  "P" "⍣"
  "ö" "←"
  "Å" "⍞"
  "ä" "→"
  "^" "⍬"
  "a" "⍺"
  "A" "⍶"
  "s" "⌈"
  ;; "S" "⌈"
  "d" "⌊"
  ;; "D" "⌊"
  "f" "_"
  ;; "F" "_"
  "g" "∇"
  "G" "⍢"
  "h" "∆"
  ;; "H" "∆"
  "j" "∘"
  "J" "⍤"
  ;; "k" "'"
  "K" "⌸"
  "l" "⎕"
  "L" "⌷"
  "å" "⍎"
  "Ö" "≡"
  "~" "⍕"
  "Ä" "≢"
  "'" "⊢"
  "*" "⊣"
  "<" "⊢"
  ">" "⊣"
  "z" "⊂"
  "Z" "⊆"
  "x" "⊃"
  ;; "X" "⊃"
  "c" "∩"
  ;; "C" "∩"
  "v" "∪"
  ;; "V" "∪"
  "b" "⊥"
  "B" "⍭"
  "n" "⊤"
  "N" "⍡"
  ;; "m" "|"
  "M" "∥"
  "," "⍝"
  ";" "⍪"
  "." "⍀"
  ":" "⍙"
  "-" "⌿"
  "_" "⍠")
(use-package dyalog-mode
  :defer 3
  :hook
  (dyalog-mode . (lambda ()
                   (set-indents 8 8 t)
                   (setq-local compile-command (concat "cat "
                                                       (buffer-file-name)
                                                       " | dyalog 2>/dev/null ")
                               dyalog-leading-spaces 0
                               electric-pair-mode 0
                               buffer-face-mode-face '(:family "Iosevka"))
                   (buffer-face-mode)))
  :config
  (add-to-list #'auto-mode-alist '("\\.apl$" . dyalog-mode)))

(add-hook 'compilation-mode-hook (lambda ()
                                   (visual-line-mode 1)))

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
    ;; (setq-local display-line-numbers 'relative)
    ))

(global-display-line-numbers-mode)

(provide 'language-specifics)
;;; language-specifics.el ends here
