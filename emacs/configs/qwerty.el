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
  (define-key company-active-map (kbd "<tab>") 'company-complete-common 'delete)
  (set-n (kbd "SPC å") 'projectile-command-map)
  (set-n (kbd "ö") 'evil-ex)
  (set-n (kbd "Ö") 'eval-expression)
  (set-n   (kbd "C-n") 'scratch-buffer)

  (define-key evil-normal-state-map (kbd "<escape>") 'evil-esc)
  (define-key evil-motion-state-map (kbd "<escape>") 'evil-esc)
  (define-key evil-visual-state-map (kbd "<escape>") 'evil-esc)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-esc)
  (define-key evil-symex-state-map (kbd "<escape>") 'evil-esc)

  (set-nvm (kbd "a") 'evil-backward-char)
  (set-nvm (kbd "d") 'evil-forward-char)
  (set-nvm (kbd "w") 'evil-previous-visual-line)
  (set-nvm (kbd "s") 'evil-next-visual-line)

  (set-nvm (kbd "C-a") 'evil-backward-word-begin)
  (set-nm  (kbd "C-d") 'evil-forward-word-begin)
  (set-v   (kbd "C-d") 'evil-forward-word-end)
  (set-nvm (kbd "C-w") (lambda () (interactive) (evil-scroll-line-up 5)))
  (set-nvm (kbd "C-s") (lambda () (interactive) (evil-scroll-line-down 5)))

  (set-nvm "W" "%")
  (set-nvm "S" "}")

  (set-nm (kbd "C-c") 'evil-yank)
  (set-nm (kbd "C-v") 'evil-paste-after)
  (set-nm (kbd "C-x") 'evil-delete)
  (set-nm (kbd "M-V") 'evil-visual-block)

  (define-key evil-emacs-state-map (kbd "TAB") 'insert-tab-at-start) ; For gui
  (define-key evil-emacs-state-map [?\t] 'insert-tab-at-start) ; For terminal
  (define-key evil-insert-state-map (kbd "TAB") 'insert-tab-at-start) ; For gui
  (define-key evil-insert-state-map [?\t] 'insert-tab-at-start) ; For terminal

  (set-nvm (kbd "<tab>") (kbd ">>")) ; For gui
  (set-nvm [?\t] (kbd ">>")) ; For terminal use
  (set-nvm (kbd "<backtab>") (kbd "<<"))
  (set-nvm (kbd "<iso-lefttab>") (kbd "<<"))
  (set-v   (kbd "<tab>") (kbd ">gv"))
  (set-v   [?\t] (kbd ">gv"))
  (set-v   (kbd "<backtab>") (kbd "<gv"))
  (set-v   (kbd "<iso-lefttab>") (kbd "<gv"))

  (set-v (kbd "\"") 'wrap-in-quotes)
  (set-v (kbd "'") 'wrap-in-apostrophes)
  (set-v (kbd "`") 'wrap-in-ticks)
  (set-v (kbd "(") 'wrap-in-parentheses)
  (set-v (kbd "{") 'wrap-in-curlies)
  (set-v (kbd "[") 'wrap-in-squares)
  ;; (set-v (kbd "<") 'wrap-in-angles) ; Interferes with outdent
  (set-v (kbd "$") 'wrap-in-dollar)

  (set-v (kbd "SPC k") 'comment-or-uncomment-region)
  (set-n (kbd "SPC k") 'comment-or-uncomment-line)

  ;; Find
  (set-n  (kbd "SPC P") 'projectile-switch-project)
  (set-n  (kbd "SPC p") 'projectile-find-file)
  (set-nm (kbd "C-f") 'evil-search-forward)
  (set-nm (kbd "C-g") (kbd ":%s/"))

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
  (set-n   (kbd "SPC O") 'olivetti-mode)

  (define-key evil-visual-state-map (kbd "SPC b") 'narrow-to-region)
  (set-n (kbd "SPC b") 'widen)

  (set-nme (kbd "C-<tab>") 'next-buffer)
  (set-nme [C-?\t] 'next-buffer)
  (set-nme (kbd "C-<backtab>") 'previous-buffer)
  (set-nme (kbd "C-<iso-lefttab>") 'previous-buffer)
  (set-nv  (kbd "SPC <tab>") 'list-buffers)

  ;; Completion-buffer
  (define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

  (evil-define-key 'visual haskell-mode-map
    (kbd "SPC r") 'hs-slime-v)

  (evil-define-key 'normal haskell-mode-map
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC a") 'lsp-execute-code-action
    (kbd "SPC f") 'lsp-ui-doc-glance
    (kbd "SPC r") 'hs-slime-n
    (kbd "<f5>") 'hs-run
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
    (kbd "SPC i") 'rustic-format-buffer)
  (evil-define-key 'visual rustic-mode-map
    (kbd "SPC i") 'rustic-format-region)
  (evil-define-key '(normal visual) rustic-mode-map
    (kbd "SPC f") 'lsp-ui-doc-glance
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC G") 'lsp-goto-type-definition
    (kbd "SPC a") 'lsp-execute-code-action
    (kbd "SPC t") 'lsp-inlay-hints-mode
    (kbd "SPC r") 'rustic-cargo-run
    (kbd "SPC R") 'compile
    (kbd "<f2>") 'lsp-rename
    (kbd "<f4>") 'rustic-popup
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal conf-toml-mode-map
    (kbd "<f5>") 'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)
 
  (evil-define-key 'normal lisp-mode-shared-map
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC f") 'describe-symbol
    (kbd "SPC r") 'eval-buffer)
  (evil-define-key 'visual lisp-mode-shared-map
    (kbd "SPC r") 'eval-region)

  (evil-define-key 'visual sh-mode-map
    (kbd "SPC r") (lambda () (interactive)
                    (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))))

  (evil-define-key 'visual c++-mode-map
    (kbd "SPC i") 'indent-region)

  (evil-define-key 'visual c-mode-ma
    (kbd "SPC i") 'indent-region)

  (evil-define-key 'normal c++-mode-map
    (kbd "SPC r") 'compile
    (kbd "SPC i") 'indent-according-to-mode
    (kbd "SPC f") 'lsp-ui-doc-glance
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC a") 'lsp-execute-code-action)
  (evil-define-key 'normal c-mode-map
    (kbd "SPC r") 'compile
    (kbd "SPC i") 'indent-according-to-mode
    (kbd "SPC f") 'lsp-ui-doc-glance
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC a") 'lsp-execute-code-action)

  (evil-define-key 'normal tuareg-mode-map
    (kbd "SPC g") 'xref-find-definitions
    (kbd "SPC a") 'lsp-execute-code-action
    (kbd "SPC f") 'lsp-ui-doc-glance
    (kbd "SPC i") 'ocamlformat)

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
    
  (setq symex--user-evil-keyspec
        '(("s" . symex-go-up)
          ("w" . symex-go-down)
          ("C-s" . evil-scroll-line-down)
          ("C-w" . evil-scroll-line-up)
          ("M-s" . symex-goto-highest)
          ("M-w" . symex-goto-lowest)
          ("d" . symex-go-forward)
          ("a" . symex-go-backward)
          ("C-d" . symex-traverse-forward)
          ("C-a" . symex-traverse-backward)
          ("D" . symex-insert-at-end)
          ("A" . symex-insert-before)
          ("SPC u d" . symex-capture-forward)
          ("SPC u a" . symex-capture-backward)
          ("SPC y d" . symex-emit-forward)
          ("SPC y a" . symex-emit-forward)
          ("v" . select-with-symex)
          ("i" . evil-insert)
          ("r" . symex-change)
          ("C-c" . symex-yank)
          ("C-x" . symex-delete)
          ("C-v" . symex-paste-after)))
  (symex-initialize))