(provide 'colemak)

(defun colemak-keymap ()
  (interactive)

  ;; Emacs stuff
  (global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-M-o") 'evil-force-normal-state)
  (define-key evil-emacs-state-map (kbd "C-M-i") 'evil-force-normal-state)
  (define-key evil-normal-state-map (kbd "C-M-i") 'evil-emacs-state)
  (global-set-key (kbd "C-S-p") 'treemacs)
  (set-nm (kbd "C-d") 'multi-vterm)
  (set-nm (kbd "C-+") 'text-scale-increase)
  (set-nm (kbd "C--") 'text-scale-decrease)
  (define-key evil-normal-state-map (kbd "SPC m") 'magit)
  (define-key evil-normal-state-map (kbd "SPC n") 'magit-blame)
  (define-key evil-normal-state-map (kbd "SPC v") 'hs-toggle-hiding)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") 'newline)
  (set-n (kbd "SPC å") 'projectile-command-map)
  (set-n (kbd "ö") 'evil-ex)
  (set-n (kbd "Ö") 'eval-expression)
  (set-n (kbd "C-n") 'scratch-buffer)

  ;; (define-key evil-normal-state-map (kbd "<escape>") 'evil-esc)
  ;; (define-key evil-motion-state-map (kbd "<escape>") 'evil-esc)
  ;; (define-key evil-visual-state-map (kbd "<escape>") 'evil-esc)
  ;; (define-key evil-insert-state-map (kbd "<escape>") 'evil-esc)
  ;; (define-key evil-symex-state-map  (kbd "<escape>") 'evil-esc)

  (define-key evil-operator-state-map "u" evil-inner-text-objects-map)
  (define-key evil-visual-state-map "u" evil-inner-text-objects-map)
  (define-key evil-operator-state-map "y" evil-outer-text-objects-map)
  (define-key evil-visual-state-map "y" evil-outer-text-objects-map)

  (set-nvm (kbd "r") 'evil-backward-char)
  (set-nvm (kbd "t") 'evil-forward-char)
  (set-nvm (kbd "f") 'evil-previous-visual-line)
  (set-nvm (kbd "s") 'evil-next-visual-line)

  (set-nvm (kbd "R") 'evil-backward-word-begin)
  (set-nm  (kbd "T") 'evil-forward-word-begin)
  (set-v   (kbd "T") 'evil-forward-word-end)
  (set-nvm (kbd "F") (lambda () (interactive) (evil-scroll-line-up 5)))
  (set-nvm (kbd "S") (lambda () (interactive) (evil-scroll-line-down 5)))

  (set-nvm (kbd "C-f") 'evil-jump-item)
  (set-nvm (kbd "C-s") 'evil-forward-item)
  (set-n   (kbd "p")   'evil-replace)
  (set-n   (kbd "P")   'evil-enter-replace-state)

  (set-nvm (kbd "n")   'evil-insert)
  (set-nvm (kbd "l")   'evil-open-below)
  (set-nvm (kbd "L")   'evil-open-above)
  (set-nvm (kbd "e")   'evil-yank)
  (set-nvm (kbd "i")   'evil-delete)
  (set-nm  (kbd "o")   'evil-paste-after)
  (set-nm  (kbd "M-v") 'evil-visual-block)

  (set-nmi  (kbd "<tab>")         'my/indent-line) ; For gui
  (set-nmi  [?\t]                 'my/indent-line) ; For terminal use
  (set-nmie (kbd "<backtab>")     'my/outdent-line)
  (set-nmie (kbd "<iso-lefttab>") 'my/outdent-line)
  (set-v    (kbd "<tab>")         'my/indent-region)
  (set-v    [?\t]                 'my/indent-region)
  (set-v    (kbd "<backtab>")     'my/outdent-region)
  (set-v    (kbd "<iso-lefttab")  'my/outdent-region)

  (set-nv (kbd "A") 'align-regexp)

  (set-v (kbd "\"") 'wrap-in-quotes)
  (set-v (kbd "'") 'wrap-in-apostrophes)
  (set-v (kbd "`") 'wrap-in-ticks)
  (set-v (kbd "(") 'wrap-in-parentheses)
  (set-v (kbd "{") 'wrap-in-curlies)
  (set-v (kbd "[") 'wrap-in-squares)
  ;; (set-v (kbd "<") 'wrap-in-angles) ; Interferes with outdent
  (set-v (kbd "$") 'wrap-in-dollar)

  (set-v (kbd "SPC u") 'comment-or-uncomment-region)
  (set-n (kbd "SPC u") 'comment-or-uncomment-line)

  ;; Find
  (set-n  (kbd "SPC P") 'projectile-switch-project)
  (set-n  (kbd "SPC p") 'projectile-find-file)

  ;; Undo and redo
  (set-nmi (kbd "C-q") 'evil-undo)
  (set-nmi (kbd "C-S-q") 'evil-redo)

  (set-nm (kbd "C-r") 'evil-insert-line)
  (set-nm (kbd "C-t") 'evil-append-line)
  (set-nm (kbd "I") 'evil-delete-line)
  (set-nm (kbd "u") nil)
  (set-nm (kbd "&") nil)
  (define-key evil-symex-state-map (kbd "a") nil)

  (set-nme (kbd "C-p") 'split-window-horizontally)
  (set-nme (kbd "C-w") 'split-window-vertically)
  (set-nme (kbd "C-S-f") 'evil-window-up)
  (set-nme (kbd "C-S-s") 'evil-window-down)
  (set-nme (kbd "C-S-r") 'evil-window-left)
  (set-nme (kbd "C-S-t") 'evil-window-right)
  (set-nme (kbd "M-F") 'evil-window-decrease-height)
  (set-nme (kbd "M-S") 'evil-window-increase-height)
  (set-nme (kbd "M-T") 'evil-window-increase-width)
  (set-nme (kbd "M-R") 'evil-window-decrease-width)
  (set-n   (kbd "SPC g") 'transpose-with-treemacs)
  (set-n   (kbd "SPC G") 'toggle-left-margin)

  (define-key evil-visual-state-map (kbd "SPC b") 'narrow-to-region)
  (set-n (kbd "SPC b") 'widen)

  (set-nme (kbd "C-<tab>") 'next-buffer)
  (set-nme [C-?\t] 'next-buffer)
  (set-nme (kbd "C-<backtab>") 'previous-buffer)
  (set-nme (kbd "C-<iso-lefttab>") 'previous-buffer)
  (set-nv  (kbd "SPC <tab>") 'get-buffer-list)

  (set-nm (kbd "-") 'evil-search-forward)
  (set-nm (kbd "_") 'evil-search-backward)
  (set-nm (kbd "m") 'evil-search-next)
  (set-nm (kbd "M") 'evil-search-prev)

  ;; Completion-buffer
  (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

  (set-n (kbd "SPC q") 'evil-quit)
  (set-n (kbd "SPC Q") 'evil-save-modified-and-close)
  (set-n (kbd "SPC w") 'save-buffer)
  (set-n (kbd "SPC f") 'save-buffer)

  (evil-define-key '(normal motion) evil-command-window-mode-map
    (kbd "C-g") 'evil-quit)

  (evil-define-key 'visual haskell-mode-map
    (kbd "SPC l") 'hs-slime-v)

  (evil-define-key 'normal haskell-mode-map
    (kbd "SPC i") 'xref-find-definitions
    (kbd "SPC n") 'lsp-execute-code-action
    (kbd "SPC e") 'lsp-ui-doc-glance
    (kbd "SPC l") 'hs-slime-n
    (kbd "<f5>") 'hs-run
    (kbd "<f2>") 'lsp-rename
    (kbd "SPC o") (lambda () (interactive)
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
    (kbd "SPC o") 'rustic-format-buffer)
  (evil-define-key 'visual rustic-mode-map
    (kbd "SPC o") 'rustic-format-region)
  (evil-define-key '(normal visual) rustic-mode-map
    (kbd "SPC e") 'lsp-ui-doc-glance
    (kbd "SPC i") 'xref-find-definitions
    (kbd "SPC I") 'lsp-goto-type-definition
    (kbd "SPC n") 'lsp-execute-code-action
    (kbd "SPC t") 'lsp-inlay-hints-mode
    (kbd "SPC l") 'rustic-cargo-run
    (kbd "SPC L") 'compile
    (kbd "<f2>") 'lsp-rename
    (kbd "<f4>") 'rustic-popup
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal conf-toml-mode-map
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "SPC l") 'eval-buffer)
  (evil-define-key 'visual emacs-lisp-mode-map
    (kbd "SPC l") 'eval-region)

  (evil-define-key '(normal visual) lisp-mode-shared-map
    (kbd "SPC i") 'describe-symbol
    (kbd "SPC o") 'indent-according-to-mode
    (kbd "SPC t") 'paredit-forward-slurp-sexp
    (kbd "SPC T") 'paredit-forward-barf-sexp
    (kbd "SPC a") 'paredit-backward-slurp-sexp
    (kbd "SPC A") 'paredit-backward-barf-sexp)

  (evil-define-key 'normal scheme-mode-map
    (kbd "SPC l") 'slime-n
    (kbd "SPC L") 'slime-buf)
  (evil-define-key 'visual scheme-mode-map
    (kbd "SPC l") 'slime-v)

  (evil-define-key 'visual sh-mode-map
    (kbd "SPC l") (lambda () (interactive)
                    (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))))

  (evil-define-key 'visual c++-mode-map
    (kbd "SPC o") 'indent-region)

  (evil-define-key 'visual c-mode-map
    (kbd "SPC o") 'indent-region)

  (evil-define-key 'normal c++-mode-map
    (kbd "SPC l") 'recompile
    (kbd "SPC L") 'compile
    (kbd "SPC o") 'indent-according-to-mode
    (kbd "SPC e") 'lsp-ui-doc-glance
    (kbd "SPC t") 'lsp-inlay-hints-mode
    (kbd "SPC i") 'xref-find-definitions
    (kbd "<f2>") 'lsp-rename
    (kbd "SPC n") 'lsp-execute-code-action)
  (evil-define-key 'normal c-mode-map
    (kbd "SPC l") 'recompile
    (kbd "SPC L") 'compile
    (kbd "SPC o") 'indent-according-to-mode
    (kbd "SPC e") 'lsp-ui-doc-glance
    (kbd "SPC t") 'lsp-inlay-hints-mode
    (kbd "SPC i") 'xref-find-definitions
    (kbd "<f2>") 'lsp-rename
    (kbd "SPC n") 'lsp-execute-code-action)

  (evil-define-key 'normal nix-mode-map
    (kbd "SPC o") 'indent-according-to-mode)
  (evil-define-key 'visual nix-mode-map
    (kbd "SPC o") 'indent-region)
  (evil-define-key '(normal visual) nix-mode-map
    (kbd "SPC i") 'xref-find-definitions
    (kbd "SPC n") 'lsp-execute-code-action
    (kbd "SPC e") 'lsp-ui-doc-glance)

  (evil-define-key 'normal tuareg-mode-map
    (kbd "SPC i") 'xref-find-definitions
    (kbd "SPC n") 'lsp-execute-code-action
    (kbd "SPC e") 'lsp-ui-doc-glance
    (kbd "<f2>") 'lsp-rename
    (kbd "SPC o") 'ocamlformat)

  (evil-define-key 'visual futhark-mode-map
    (kbd "SPC o") 'indent-region)
  (evil-define-key 'normal futhark-mode-map
    (kbd "SPC o") 'indent-according-to-mode
    (kbd "SPC i") 'xref-find-definitions
    (kbd "SPC n") 'lsp-execute-code-action
    (kbd "SPC e") 'lsp-ui-doc-glance
    (kbd "SPC t") 'lsp-ui-sideline-mode
    (kbd "SPC I") 'lsp-goto-type-definition
    (kbd "SPC l") 'compile
    (kbd "<f2>") 'lsp-rename
    (kbd "<f5>") 'dap-debug)

  (evil-define-key 'insert vterm-mode-map
    (kbd "C-V") 'vterm-yank)

  (evil-define-key '(normal emacs) vterm-mode-map
    (kbd "C-S-F") 'windmove-up
    (kbd "C-S-S") 'windmove-down
    (kbd "C-S-R") 'windmove-left
    (kbd "C-S-T") 'windmove-right
    (kbd "C-S-V") 'vterm-yank
    (kbd "C-v") 'vterm-yank)

  (evil-define-key 'normal treemacs-mode-map
    (kbd "SPC") 'treemacs-TAB-action
    (kbd "C-<tab>") 'treemacs-switch-workspace
    (kbd "x") 'treemacs-delete-file
    (kbd "<delete>") 'treemacs-delete-file
    (kbd "r") nil
    (kbd "t") nil)

  (evil-define-key '(normal emacs motion) Buffer-menu-mode-map
    (kbd "<return>") 'Buffer-menu-this-window
    (kbd "C-n") 'nuke-all-buffers)

  (setq symex--user-evil-keyspec
        '(("s" . symex-go-up)
          ("f" . symex-go-down)
          ("S" . evil-scroll-line-down)
          ("F" . evil-scroll-line-up)
          ("M-s" . symex-goto-highest)
          ("M-f" . symex-goto-lowest)
          ("t" . symex-go-forward)
          ("r" . symex-go-backward)
          ("T" . symex-traverse-forward)
          ("R" . symex-traverse-backward)
          ("C-t" . symex-insert-at-end)
          ("C-r" . symex-insert-before)
          ("a" . nil)
          ("SPC u t" . symex-capture-forward)
          ("SPC u r" . symex-capture-backward)
          ("SPC y t" . symex-emit-forward)
          ("SPC y r" . symex-emit-forward)
          ("v" . select-with-symex)
          ("n" . evil-insert)
          ("p" . symex-change)
          ("e" . symex-yank)
          ("i" . symex-delete)
          ("o" . symex-paste-after)
          ("O" . symex-paste-before)))
  (symex-initialize))
