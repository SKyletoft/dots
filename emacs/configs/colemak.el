(provide 'colemak)

(defun colemak-keymap ()
  (interactive)

  ;; Emacs stuff
  (global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-M-o") 'evil-force-normal-state)
  (define-key evil-emacs-state-map (kbd "C-M-i") 'evil-force-normal-state)
  (define-key evil-normal-state-map (kbd "C-M-i") 'evil-emacs-state)
  (define-key evil-normal-state-map (kbd "z x") 'execute-extended-command)
  (global-set-key (kbd "C-S-p") 'treemacs)
  (evil-define-key '(normal motion) 'global
    (kbd "C-d") 'multi-vterm
    (kbd "C-+") 'text-scale-increase
    (kbd "C--") 'text-scale-decrease)
  (define-key company-active-map (kbd "<tab>") 'my/snippet-complete-or-indent)
  (define-key company-active-map (kbd "<return>") 'newline)
  (evil-define-key 'normal 'global
    (kbd "SPC m") 'magit
    (kbd "SPC n") 'magit-blame
    (kbd "SPC v") 'hs-toggle-hiding
    (kbd "SPC å") 'projectile-command-map
    (kbd "ö") 'evil-ex
    (kbd "Ö") 'eval-expression
    (kbd "C-n") 'scratch-buffer)

  (evil-define-key 'operator 'global
    (kbd "u") evil-inner-text-objects-map
    (kbd "y") evil-outer-text-objects-map)
  (evil-define-key 'visual 'global
    (kbd "u") evil-inner-text-objects-map
    (kbd "y") evil-outer-text-objects-map)

  (evil-define-key '(normal visual motion) 'global
    (kbd "r") 'evil-backward-char
    (kbd "t") 'evil-forward-char
    (kbd "f") 'evil-previous-visual-line
    (kbd "s") 'evil-next-visual-line)

  (evil-define-key '(normal motion) 'global
    (kbd "T") 'evil-forward-word-begin)
  (evil-define-key 'visual 'global
    (kbd "T") 'evil-forward-word-end)
  (evil-define-key '(normal visual motion) 'global
    (kbd "F") (lambda () (interactive) (evil-scroll-line-up 5))
    (kbd "S") (lambda () (interactive) (evil-scroll-line-down 5))
    (kbd "R") 'evil-backward-word-begin)

  (evil-define-key '(normal visual motion) 'global
    (kbd "C-f") 'evil-jump-item
    (kbd "C-s") 'evil-jump-paragraph)
  (evil-define-key 'normal 'global
    (kbd "p")   'evil-replace
    (kbd "P")   'evil-enter-replace-state)

  (evil-define-key '(normal visual motion) 'global
    (kbd "n")   'evil-insert
    (kbd "l")   'evil-open-below
    (kbd "L")   'evil-open-above
    (kbd "e")   'evil-yank
    (kbd "i")   'evil-delete)
  (evil-define-key '(normal motion) 'global
    (kbd "o")   'evil-paste-after
    (kbd "M-v") 'evil-visual-block)

  (evil-define-key '(normal motion insert) 'global
    (kbd "<tab>")         'my/indent-line  ; For gui
    [?\t]                 'my/indent-line) ; For terminal use
  (evil-define-key '(normal motion insert emacs) 'global
    (kbd "<backtab>")     'my/outdent-line
    (kbd "<iso-lefttab>") 'my/outdent-line)
  (evil-define-key 'visual 'global
    (kbd "<tab>")         'my/indent-region
    [?\t]                 'my/indent-region
    (kbd "<backtab>")     'my/outdent-region
    (kbd "<iso-lefttab")  'my/outdent-region)

  (evil-define-key '(normal visual) 'global
    (kbd "A") 'align-regexp)

  (evil-define-key 'visual 'global
    (kbd "\"") 'wrap-in-quotes
    (kbd "'") 'wrap-in-apostrophes
    (kbd "`") 'wrap-in-ticks
    (kbd "(") 'wrap-in-parentheses
    (kbd "{") 'wrap-in-curlies
    (kbd "[") 'wrap-in-squares
    (kbd "<") 'wrap-in-angles
    (kbd "$") 'wrap-in-dollar)

  (set-v (kbd "SPC u") 'comment-or-uncomment-region)
  (set-n (kbd "SPC u") 'comment-or-uncomment-line)

  ;; Find
  (evil-define-key 'normal 'global
    (kbd "SPC P") 'projectile-switch-project
    (kbd "SPC p") 'projectile-find-file
    (kbd "SPC d") 'ripgrep-regexp)

  ;; Undo and redo
  (evil-define-key '(normal motion insert) 'global
    (kbd "C-q") 'evil-undo
    (kbd "C-S-q") 'evil-redo)

  (evil-define-key '(normal motion) 'global
    (kbd "C-r") 'evil-insert-line
    (kbd "C-t") 'evil-append-line
    (kbd "I") 'evil-delete-line
    (kbd "u") nil
    (kbd "&") nil)

  (evil-define-key '(normal motion emacs) 'global
    (kbd "C-p") 'split-window-horizontally
    (kbd "C-w") 'split-window-vertically
    (kbd "C-S-f") 'evil-window-up
    (kbd "C-S-s") 'evil-window-down
    (kbd "C-S-r") 'evil-window-left
    (kbd "C-S-t") 'evil-window-right
    (kbd "M-F") 'evil-window-decrease-height
    (kbd "M-S") 'evil-window-increase-height
    (kbd "M-T") 'evil-window-increase-width
    (kbd "M-R") 'evil-window-decrease-width)
  (evil-define-key 'normal 'global
    (kbd "SPC g") 'transpose-with-treemacs
    (kbd "SPC G") 'toggle-left-margin)

  (evil-define-key 'normal 'global
    (kbd "SPC b") 'widen)
  (evil-define-key 'visual 'global
    (kbd "SPC b") 'narrow-to-region)

  (evil-define-key '(normal motion emacs) 'global
    (kbd "C-<tab>") 'next-buffer
    [C-?\t] 'next-buffer
    (kbd "C-<backtab>") 'previous-buffer
    (kbd "C-<iso-lefttab>") 'previous-buffer)
  (evil-define-key '(normal visual) 'global
    (kbd "SPC <tab>") 'get-buffer-list)

  (evil-define-key '(normal motion) 'global
    (kbd "-") 'evil-search-forward
    (kbd "_") 'evil-search-backward
    (kbd "m") 'evil-search-next
    (kbd "M") 'evil-search-prev)

  ;; Completion-buffer
  (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

  (evil-define-key 'normal 'global
    (kbd "SPC q") 'evil-quit
    (kbd "SPC Q") 'evil-save-modified-and-close
    (kbd "SPC w") 'save-buffer
    (kbd "SPC f") 'save-buffer)

  (evil-define-key '(normal motion) evil-command-window-mode-map
    (kbd "C-g") 'evil-quit)

  ;; Default language with lsp-mode bindings
  (defmacro lang-with-lsp (map)
    `(progn (evil-define-key 'visual ,map
              (kbd "SPC o") 'indent-region
              (kbd "SPC O") 'lsp-format-region)
            (evil-define-key 'normal ,map
              (kbd "SPC o") 'indent-according-to-mode
              (kbd "SPC O") 'lsp-format-buffer)
            (evil-define-key '(normal visual) ,map
              (kbd "SPC l") 'recompile
              (kbd "SPC L") 'compile
              (kbd "SPC e") 'lsp-ui-doc-glance
              (kbd "SPC i") 'xref-find-definitions
              (kbd "SPC I") 'lsp-goto-type-definition
              (kbd "SPC n") 'lsp-execute-code-action
              (kbd "SPC t") 'lsp-inlay-hints-mode
              (kbd "SPC v") 'gud-break
              (kbd "<f2>")  'lsp-rename
              (kbd "<f5>")  'dap-debug)))

  (lang-with-lsp js-mode-map)
  (lang-with-lsp pest-mode-map)
  (lang-with-lsp csharp-ts-mode-map)
  (lang-with-lsp nix-mode-map)
  (lang-with-lsp futhark-mode-map)
  (lang-with-lsp c-mode-map)
  (lang-with-lsp c++-mode-map)
  (lang-with-lsp java-ts-mode-map)
  (lang-with-lsp kotlin-mode-map)
  (lang-with-lsp tuareg-mode-map)
  (lang-with-lsp haskell-mode-map)
  (lang-with-lsp rustic-mode-map)
  (lang-with-lsp rust-ts-mode-map)
  (lang-with-lsp erlang-mode-map)

  (evil-define-key 'visual haskell-mode-map
    (kbd "SPC l") 'hs-slime-v)
  (evil-define-key 'normal haskell-mode-map
    (kbd "SPC l") 'hs-slime-n
    (kbd "SPC O") (lambda () (interactive)
                    (save-buffer)
                    (shell-command (concat "hindent "
                                           (buffer-file-name)
                                           " && stylish-haskell -i "
                                           (buffer-file-name)))
                    (revert-buffer t t t))
    (kbd "<f5>")  'hs-run)

  (evil-define-key 'normal idris-mode-map
    (kbd "SPC c") 'idris-case-dwim
    (kbd "SPC d") 'idris-type-at-point
    (kbd "SPC f") 'idris-type-at-point
    (kbd "SPC l") 'idris-make-lemma
    (kbd "SPC m") 'idris-add-missing
    (kbd "SPC r") 'idris-load-file
    (kbd "SPC s") 'idris-type-search
    (kbd "SPC t") 'idris-make-lemma
    (kbd "SPC g") 'idris-proof-search
    (kbd "SPC G") 'idris-generate-def
    (kbd "SPC h") 'toggle-hole)

  (evil-define-key 'normal rustic-mode-map
    (kbd "SPC O") 'rustic-format-buffer)
  (evil-define-key 'visual rustic-mode-map
    (kbd "SPC O") 'rustic-format-buffer)
  (evil-define-key '(normal visual) rustic-mode-map
    (kbd "SPC l")  'rustic-cargo-run
    (kbd "<f4>")   'rustic-popup
    (kbd "<f5>")   'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal conf-toml-mode-map
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "SPC o") 'indent-according-to-mode
    (kbd "SPC l") 'eval-buffer)
  (evil-define-key 'visual emacs-lisp-mode-map
    (kbd "SPC o") 'indent-region
    (kbd "SPC l") 'eval-region)

  (evil-define-key '(normal visual) lisp-mode-shared-map
    (kbd "SPC i") 'describe-symbol
    (kbd "SPC t") 'paredit-forward-slurp-sexp
    (kbd "SPC T") 'paredit-forward-barf-sexp
    (kbd "SPC a") 'paredit-backward-slurp-sexp
    (kbd "SPC A") 'paredit-backward-barf-sexp)

  (evil-define-key 'normal scheme-mode-map
    (kbd "SPC o") 'indent-according-to-mode
    (kbd "SPC l") 'slime-n
    (kbd "SPC L") 'slime-buf)
  (evil-define-key 'visual scheme-mode-map
    (kbd "SPC o") 'indent-region
    (kbd "SPC l") 'slime-v)

  (evil-define-key 'visual sh-mode-map
    (kbd "SPC l") (lambda () (interactive)
                    (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))))

  (evil-define-key 'normal c-mode-map
    (kbd "SPC O") 'save-and-clang-format-buffer
    (kbd "<f5>") 'gdb)
  (evil-define-key 'normal c++-mode-map
    (kbd "SPC O") 'save-and-clang-format-buffer
    (kbd "<f5>") 'gdb)

  (evil-define-key 'normal java-ts-mode-map
    (kbd "SPC O") 'save-and-clang-format-buffer
    (kbd "<f5>") 'jdb)

  (evil-define-key 'normal kotlin-mode-map
    (kbd "SPC O") 'save-and-clang-format-buffer
    (kbd "<f5>") 'jdb)

  (evil-define-key 'normal js-mode-map
    (kbd "SPC O") 'save-and-clang-format-buffer)

  (evil-define-key 'normal tuareg-mode-map
    (kbd "SPC O") 'ocamlformat)

  (evil-define-key 'normal gud-mode-map
    (kbd "SPC t") 'gud-step
    (kbd "SPC s") 'gud-next
    (kbd "SPC r") 'gud-stepi
    (kbd "SPC p") 'gud-cont
    (kbd "SPC f") 'gud-finish)

  (evil-define-key 'insert vterm-mode-map
    (kbd "C-V") 'vterm-yank
    (kbd "C-g") 'vterm--self-insert)

  (evil-define-key '(normal emacs) vterm-mode-map
    (kbd "C-S-F") 'windmove-up
    (kbd "C-S-S") 'windmove-down
    (kbd "C-S-R") 'windmove-left
    (kbd "C-S-T") 'windmove-right
    (kbd "C-S-V") 'vterm-yank
    (kbd "C-v")   'vterm-yank)

  (evil-define-key 'normal treemacs-mode-map
    (kbd "SPC")      'treemacs-TAB-action
    (kbd "C-<tab>")  'treemacs-switch-workspace
    (kbd "x")        'treemacs-delete-file
    (kbd "<delete>") 'treemacs-delete-file
    (kbd "r")        nil
    (kbd "t")        nil)

  (evil-define-key '(normal emacs motion) Buffer-menu-mode-map
    (kbd "<return>") 'Buffer-menu-this-window
    (kbd "C-n") 'nuke-all-buffers)

  (evil-define-key 'normal pdf-view-mode-map
    (kbd "SPC g") 'pdf-view-goto-page
    (kbd "SPC G") 'pdf-view-goto-label
    (kbd "f")     'pdf-view-previous-page-command
    (kbd "r")     'pdf-view-previous-page-command
    (kbd "s")     'pdf-view-next-page-command
    (kbd "t")     'pdf-view-next-page-command))
