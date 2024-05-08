(provide 'qwerty)

(defun qwerty-keymap ()
  (interactive)

  ;; Emacs stuff
  (global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-M-o") 'evil-force-normal-state)
  (define-key evil-emacs-state-map (kbd "C-M-i") 'evil-force-normal-state)
  (define-key evil-normal-state-map (kbd "C-M-i") 'evil-emacs-state)
  (global-set-key (kbd "C-M-p") 'treemacs)
  (set-nm (kbd "C-t") 'multi-vterm)
  (set-nm (kbd "C-+") 'text-scale-increase)
  (set-nm (kbd "C--") 'text-scale-decrease)
  (define-key evil-normal-state-map (kbd "SPC m") 'magit)
  (define-key evil-normal-state-map (kbd "SPC n") 'magit-blame)
  (define-key evil-normal-state-map (kbd "SPC v") 'hs-toggle-hiding)
  (define-key company-active-map (kbd "<tab>") 'my/snippet-complete-or-indent)
  (define-key company-active-map (kbd "<return>") 'newline)
  (set-n (kbd "SPC å") 'projectile-command-map)
  (set-n (kbd "ö") 'evil-ex)
  (set-n (kbd "Ö") 'eval-expression)
  (set-n (kbd "C-n") 'scratch-buffer)

  (set-nvm (kbd "a") 'evil-backward-char)
  (set-nvm (kbd "d") 'evil-forward-char)
  (set-nvm (kbd "w") 'evil-previous-visual-line)
  (set-nvm (kbd "s") 'evil-next-visual-line)

  (set-nvm (kbd "C-a") 'evil-backward-word-begin)
  (set-nm  (kbd "C-d") 'evil-forward-word-begin)
  (set-v   (kbd "C-d") 'evil-forward-word-end)
  (set-nvm (kbd "C-w") (lambda () (interactive) (evil-scroll-line-up 5)))
  (set-nvm (kbd "C-s") (lambda () (interactive) (evil-scroll-line-down 5)))

  (set-nvm (kbd "W") 'evil-jump-item)
  (set-nvm (kbd "S") 'evil-jump-paragraph)

  (set-nm (kbd "C-c") 'evil-yank)
  (set-nm (kbd "C-v") 'evil-paste-after)
  (set-nm (kbd "C-x") 'evil-delete)
  (set-nm (kbd "M-V") 'evil-visual-block)

  (set-nmi  (kbd "<tab>")         'my/indent-line) ; For gui
  (set-nmi  [?\t]                 'my/indent-line) ; For terminal use
  (set-nmie (kbd "<backtab>")     'my/outdent-line)
  (set-nmie (kbd "<iso-lefttab>") 'my/outdent-line)
  (set-v    (kbd "<tab>")         'my/indent-region)
  (set-v    [?\t]                 'my/indent-region)
  (set-v    (kbd "<backtab>")     'my/outdent-region)
  (set-v    (kbd "<iso-lefttab")  'my/outdent-region)

  (set-v (kbd "M-r") 'align-regexp)

  (set-v (kbd "\"") 'wrap-in-quotes)
  (set-v (kbd "'") 'wrap-in-apostrophes)
  (set-v (kbd "`") 'wrap-in-ticks)
  (set-v (kbd "(") 'wrap-in-parentheses)
  (set-v (kbd "{") 'wrap-in-curlies)
  (set-v (kbd "[") 'wrap-in-squares)
  (set-v (kbd "<") 'wrap-in-angles) ; Interferes with outdent
  (set-v (kbd "$") 'wrap-in-dollar)

  (set-v (kbd "SPC k") 'comment-or-uncomment-region)
  (set-n (kbd "SPC k") 'comment-or-uncomment-line)

  ;; Find
  (set-n  (kbd "SPC P") 'projectile-switch-project)
  (set-n  (kbd "SPC p") 'projectile-find-file)
  (set-nm (kbd "C-f") 'evil-search-forward)

  ;; Undo and redo
  (set-nmi (kbd "C-z") 'evil-undo)
  (set-nmi (kbd "C-r") 'evil-redo)

  (set-nm "A" 'evil-insert-line)
  (set-nm "D" 'evil-append-line)
  (set-nm (kbd "X") 'evil-delete-line)
  (set-nm (kbd "u") nil)
  (set-nm (kbd "&") nil)

  (set-nme (kbd "C-e") 'split-window-horizontally)
  (set-nme (kbd "C-q") 'split-window-vertically)
  (set-nme (kbd "C-M-w") 'evil-window-up)
  (set-nme (kbd "C-M-s") 'evil-window-down)
  (set-nme (kbd "C-M-a") 'evil-window-left)
  (set-nme (kbd "C-M-d") 'evil-window-right)
  (set-nme (kbd "M-W") 'evil-window-decrease-height)
  (set-nme (kbd "M-S") 'evil-window-increase-height)
  (set-nme (kbd "M-D") 'evil-window-increase-width)
  (set-nme (kbd "M-A") 'evil-window-decrease-width)
  (set-n   (kbd "SPC o") 'transpose-with-treemacs)
  (set-n   (kbd "SPC O") 'toggle-left-margin)

  (define-key evil-visual-state-map (kbd "SPC b") 'narrow-to-region)
  (set-n (kbd "SPC b") 'widen)

  (set-nme (kbd "C-<tab>") 'next-buffer)
  (set-nme [C-?\t] 'next-buffer)
  (set-nme (kbd "C-<backtab>") 'previous-buffer)
  (set-nme (kbd "C-<iso-lefttab>") 'previous-buffer)
  (set-nv  (kbd "SPC <tab>") 'get-buffer-list)

  (set-n (kbd "SPC q") 'evil-quit)
  (set-n (kbd "SPC Q") 'evil-save-modified-and-close)
  (set-n (kbd "SPC w") 'save-buffer)

  ;; Completion-buffer
  (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

  (evil-define-key '(normal motion) evil-command-window-mode-map
    (kbd "C-g") 'evil-quit)

  ;; Default language with lsp-mode bindings
  (defmacro lang-with-lsp (map)
    `(progn (evil-define-key 'visual ,map
              (kbd "SPC i") 'indent-region
              (kbd "SPC I") 'lsp-format-region)
            (evil-define-key 'normal ,map
              (kbd "SPC i") 'indent-according-to-mode
              (kbd "SPC I") (lambda () (interactive)
                              (save-buffer)
                              (shell-command (concat "clang-format -i "
                                                     (buffer-file-name)))
                              (revert-buffer t t t)))
            (evil-define-key '(normal visual) ,map
              (kbd "SPC r") 'recompile
              (kbd "SPC R") 'compile
              (kbd "SPC f") 'lsp-ui-doc-glance
              (kbd "SPC g") 'xref-find-definitions
              (kbd "SPC G") 'lsp-goto-type-definition
              (kbd "SPC a") 'lsp-execute-code-action
              (kbd "SPC t") 'lsp-inlay-hints-mode
              (kbd "SPC v") 'gud-break
              (kbd "<f2>") 'lsp-rename)))

  (lang-with-lsp js-mode-map)
  (lang-with-lsp js-ts-mode-map)
  (lang-with-lsp typescript-mode-map)
  (lang-with-lsp typescript-ts-mode-map)
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

  (evil-define-key 'normal makefile-gmake-mode-map
    (kbd "SPC r") 'recompile
    (kbd "SPC R") 'compile)

  (evil-define-key 'visual haskell-mode-map
    (kbd "SPC r") 'hs-slime-v)
  (evil-define-key 'normal haskell-mode-map
    (kbd "SPC r") 'hs-slime-n
    (kbd "SPC i") (lambda () (interactive)
                    (save-buffer)
                    (shell-command (concat "hindent "
                                           (buffer-file-name)
                                           " && stylish-haskell -i "
                                           (buffer-file-name)))
                    (revert-buffer t t t)))

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
    (kbd "SPC I") 'rustic-format-buffer)
  (evil-define-key 'visual rustic-mode-map
    (kbd "SPC I") 'rustic-format-region)
  (evil-define-key '(normal visual) rustic-mode-map
    (kbd "SPC r") 'rustic-cargo-run
    (kbd "<f4>") 'rustic-popup
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal conf-toml-mode-map
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "SPC r") 'eval-buffer)
  (evil-define-key 'visual emacs-lisp-mode-map
    (kbd "SPC r") 'eval-region)

  (evil-define-key 'normal lisp-mode-shared-map
    (kbd "SPC i") 'indent-according-to-mode)
  (evil-define-key 'visual lisp-mode-shared-map
    (kbd "SPC i") 'indent-region)
  (evil-define-key '(normal visual) lisp-mode-shared-map
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC f") 'describe-symbol
    (kbd "SPC d") 'paredit-forward-slurp-sexp
    (kbd "SPC D") 'paredit-forward-barf-sexp
    (kbd "SPC a") 'paredit-backward-slurp-sexp
    (kbd "SPC A") 'paredit-backward-barf-sexp)

  (evil-define-key 'normal scheme-mode-map
    (kbd "SPC r") 'slime-n
    (kbd "SPC R") 'slime-buf)
  (evil-define-key 'visual scheme-mode-map
    (kbd "SPC r") 'slime-v)

  (evil-define-key 'visual sh-mode-map
    (kbd "SPC r") (lambda () (interactive)
                    (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))))

  (evil-define-key 'normal c-mode-map
    (kbd "<f5>") 'gdb)
  (evil-define-key 'normal c++-mode-map
    (kbd "<f5>") 'gdb)

  (evil-define-key 'normal java-ts-mode-map
    (kbd "<f5>") 'jdb)

  (evil-define-key 'normal kotlin-mode-map
    (kbd "<f5>") 'jdb)

  (evil-define-key 'normal tuareg-mode-map
    (kbd "SPC I") 'ocamlformat)

  (evil-define-key 'normal gud-mode-map
    (kbd "SPC d") 'gud-step
    (kbd "SPC s") 'gud-next
    (kbd "SPC a") 'gud-stepi
    (kbd "SPC e") 'gud-cont
    (kbd "SPC w") 'gud-finish)

  (evil-define-key 'insert vterm-mode-map
    (kbd "C-V") 'vterm-yank)

  (evil-define-key '(normal emacs) vterm-mode-map
    (kbd "C-M-w") 'windmove-up
    (kbd "C-M-s") 'windmove-down
    (kbd "C-M-a") 'windmove-left
    (kbd "C-M-d") 'windmove-right
    (kbd "C-V") 'vterm-yank
    (kbd "C-v") 'vterm-yank)

  (evil-define-key 'normal treemacs-mode-map
    (kbd "SPC") 'treemacs-TAB-action
    (kbd "C-<tab>") 'treemacs-switch-workspace
    (kbd "x") 'treemacs-delete-file
    (kbd "<delete>") 'treemacs-delete-file
    (kbd "a") nil
    (kbd "d") nil)

  (evil-define-key '(normal emacs motion) Buffer-menu-mode-map
    (kbd "<return>") 'Buffer-menu-this-window
    (kbd "C-n") 'nuke-all-buffers)

  (evil-define-key 'normal pdf-view-mode-map
    (kbd "SPC g") 'pdf-view-goto-page
    (kbd "SPC G") 'pdf-view-goto-label
    (kbd "w") 'pdf-view-previous-page-command
    (kbd "a") 'pdf-view-previous-page-command
    (kbd "s") 'pdf-view-next-page-command
    (kbd "d") 'pdf-view-next-page-command))
