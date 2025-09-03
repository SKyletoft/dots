;;; qwerty.el --- My keymap on qwerty keyboards  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun qwerty-keymap ()
  (interactive)

  ;; Emacs stuff
  (global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "C-M-o") 'evil-force-normal-state)
  (define-key evil-emacs-state-map (kbd "C-M-i") 'evil-force-normal-state)
  (define-key evil-normal-state-map (kbd "C-M-i") 'evil-emacs-state)
  (global-set-key (kbd "C-M-p") 'treemacs)
  (evil-define-key '(normal visual motion) 'global
    (kbd "C-t") 'multi-vterm
    (kbd "C-+") 'text-scale-increase
    (kbd "C--") 'text-scale-decrease)
  (evil-define-key 'normal 'global
    (kbd "SPC m") 'magit
    (kbd "SPC n") 'magit-blame
    (kbd "SPC v") 'hs-toggle-hiding
    (kbd "ö")     'evil-ex
    (kbd "Ö")     'eval-expression
    (kbd "·")     'run-command
    (kbd "C-n")   'scratch-buffer
    (kbd "SPC R") 'compile
    (kbd "SPC r") 'recompile)

  (evil-define-key 'operator 'global
    (kbd "o") evil-outer-text-objects-map)
  (evil-define-key 'visual 'global
    (kbd "o") evil-outer-text-objects-map)

  (evil-define-key '(normal visual) 'global
    (kbd "μ") 'whitespace-mode)

  (evil-define-key '(normal visual motion) 'global
    (kbd "a") 'evil-backward-char
    (kbd "d") 'evil-forward-char
    (kbd "w") 'evil-previous-visual-line
    (kbd "s") 'evil-next-visual-line)

  (evil-define-key '(normal motion) 'global
    (kbd "C-d") 'evil-forward-word-begin)
  (evil-define-key 'visual 'global
    (kbd "C-d") 'evil-forward-word-end)
  (evil-define-key '(normal visual motion) 'global
    (kbd "C-a") 'evil-backward-word-begin
    (kbd "M-w") 'up-five
    (kbd "M-s") 'down-five
    (kbd "C-w") 'evil-backward-paragraph
    (kbd "C-s") 'evil-forward-paragraph)

  (evil-define-key '(normal visual motion) 'global
    (kbd "W") 'evil-jump-item
    (kbd "S") 'evil-jump-paragraph)

  (evil-define-key '(normal motion) 'global
    (kbd "C-c") 'evil-yank
    (kbd "c")   'evil-yank
    (kbd "C-v") 'evil-paste-after
    (kbd "C-x") 'evil-delete
    (kbd "x")   'evil-delete
    (kbd "M-V") 'evil-visual-block)
  (evil-define-key 'visual 'global
    (kbd "C-v") 'evil-visual-paste)

  (evil-define-key '(normal motion) 'global
    (kbd "<tab>") 'my/indent-line
    [?\t]         'my/indent-line) ; For terminal use
  (evil-define-key '(normal motion insert emacs) 'global
    (kbd "<backtab>")     'my/outdent-line
    (kbd "<iso-lefttab>") 'my/outdent-line)
  (evil-define-key 'visual 'global
    (kbd "<tab>")         'my/indent-region
    [?\t]                 'my/indent-region
    (kbd "<backtab>")     'my/outdent-region
    (kbd "<iso-lefttab>") 'my/outdent-region)

  (evil-define-key 'visual 'global
    (kbd "M-r") 'align-regexp)

  (evil-define-key 'visual 'global
    (kbd "\"") 'wrap-in-quotes
    (kbd "'") 'wrap-in-apostrophes
    (kbd "`") 'wrap-in-ticks
    (kbd "(") 'wrap-in-parentheses
    (kbd "{") 'wrap-in-curlies
    (kbd "[") 'wrap-in-squares
    (kbd "<") 'wrap-in-angles
    (kbd "$") 'wrap-in-dollar)

  (evil-define-key 'visual 'global
    (kbd "SPC k") 'comment-or-uncomment-region)
  (evil-define-key 'normal 'global
    (kbd "SPC k") 'comment-or-uncomment-line)

  ;; Find
  (evil-define-key 'normal 'global
    (kbd "SPC P")   'project-switch-project
    (kbd "SPC p")   'project-find-file
    (kbd "SPC SPC") 'project-find-regexp
    (kbd "M-p")     'find-file)

  ;; Undo and redo
  (evil-define-key '(normal motion insert) 'global
    (kbd "C-z") 'evil-undo
    (kbd "C-r") 'evil-redo)

  (evil-define-key '(normal motion) 'global
    (kbd "A") 'evil-insert-line
    (kbd "D") 'evil-append-line
    (kbd "X") 'evil-delete-line
    (kbd "u") nil
    (kbd "&") nil)

  (evil-define-key '(normal motion emacs) 'global
    (kbd "C-e")   'split-window-horizontally
    (kbd "C-q")   'split-window-vertically
    (kbd "C-M-w") 'evil-window-up
    (kbd "C-M-s") 'evil-window-down
    (kbd "C-M-a") 'evil-window-left
    (kbd "C-M-d") 'evil-window-right
    (kbd "M-W")   'evil-window-decrease-height
    (kbd "M-S")   'evil-window-increase-height
    (kbd "M-D")   'evil-window-increase-width
    (kbd "M-A")   'evil-window-decrease-width)
  (evil-define-key 'normal 'global
    (kbd "SPC o") 'transpose-with-treemacs
    (kbd "SPC O") 'my/toggle-left-margin)

  (evil-define-key 'visual 'global
    (kbd "SPC b") 'narrow-to-region)
  (evil-define-key 'normal 'global
    (kbd "SPC b") 'widen)

  (evil-define-key '(normal motion emacs) 'global
    (kbd "C-<tab>")         'next-buffer
    [C-?\t]                 'next-buffer
    (kbd "C-<backtab>")     'previous-buffer
    (kbd "C-<iso-lefttab>") 'previous-buffer)
  (evil-define-key '(normal visual) 'global
    (kbd "SPC <tab>") 'get-buffer-list  ; GUI
    (kbd "SPC TAB")   'get-buffer-list) ; TUI

  (evil-define-key 'normal 'global
    (kbd "SPC q") 'evil-quit
    (kbd "SPC Q") 'evil-save-modified-and-close
    (kbd "SPC w") 'save-buffer)

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
              (kbd "SPC I") 'lsp-format-buffer)
            (evil-define-key '(normal visual) ,map
              (kbd "SPC f")   'lsp-ui-doc-glance
              (kbd "SPC F")   'lsp-describe-thing-at-point
              (kbd "SPC g")   'xref-find-definitions
              (kbd "SPC G")   'lsp-goto-type-definition
              (kbd "SPC a")   'lsp-execute-code-action
              (kbd "SPC t")   'lsp-inlay-hints-mode
              (kbd "SPC v")   'gud-break
              (kbd "SPC e")   'lsp-treemacs-errors-list
              (kbd "<f2>")    'lsp-rename
              (kbd "<f5>")    'dap-debug
              (kbd "SPC M-n") 'flycheck-next-error
              (kbd "SPC M-p") 'flycheck-previous-error)))

  (lang-with-lsp js-mode-map)
  (lang-with-lsp js-ts-mode-map)
  (lang-with-lsp html-ts-mode-map)
  (lang-with-lsp css-ts-mode-map)
  (lang-with-lsp json-ts-mode-map)
  (lang-with-lsp typescript-mode-map)
  (lang-with-lsp typescript-ts-mode-map)
  (lang-with-lsp pest-mode-map)
  (lang-with-lsp csharp-ts-mode-map)
  (lang-with-lsp nix-ts-mode-map)
  (lang-with-lsp futhark-mode-map)
  (lang-with-lsp c-mode-map)
  (lang-with-lsp c++-mode-map)
  (lang-with-lsp c-ts-mode-map)
  (lang-with-lsp c++-ts-mode-map)
  (lang-with-lsp java-ts-mode-map)
  (lang-with-lsp kotlin-mode-map)
  (lang-with-lsp tuareg-mode-map)
  (lang-with-lsp haskell-ts-mode-map)
  (lang-with-lsp haskell-mode-map)
  (lang-with-lsp rustic-mode-map)
  (lang-with-lsp rust-ts-mode-map)
  (lang-with-lsp erlang-mode-map)
  (lang-with-lsp bash-ts-mode-map)
  (lang-with-lsp sh-mode-map)
  (lang-with-lsp python-ts-mode-map)
  (lang-with-lsp dafny-mode-map)
  (lang-with-lsp glsl-mode-map)
  (lang-with-lsp roc-ts-mode-map)
  (lang-with-lsp swift-ts-mode-map)
  (lang-with-lsp typst-ts-mode-map)
  (lang-with-lsp go-ts-mode-map)

  (evil-define-key '(normal visual) markdown-mode-map
    (kbd "SPC a") 'ispell-word
    (kbd "SPC A") 'ispell
    (kbd "SPC i") (kbd "vipgq"))
  (evil-define-key '(normal visual) latex-mode-map
    (kbd "SPC a") 'ispell-word
    (kbd "SPC A") 'ispell
    (kbd "SPC i") (kbd "vipgq"))
  (evil-define-key '(normal visual) typst-ts-mode-map
    (kbd "SPC a") 'ispell-word
    (kbd "SPC A") 'ispell
    (kbd "SPC i") (kbd "vipgq"))

  (evil-define-key 'visual haskell-ts-mode-map
    (kbd "SPC r") 'hs-slime-v)
  (evil-define-key 'normal haskell-ts-mode-map
    (kbd "SPC t") 'lsp-lens-mode
    (kbd "SPC r") 'hs-slime-n
    (kbd "<f5>")  'ghci)

  (evil-define-key 'visual haskell-mode-map
    (kbd "SPC r") 'hs-slime-v)
  (evil-define-key 'normal haskell-mode-map
    (kbd "SPC t") 'lsp-lens-mode
    (kbd "SPC r") 'hs-slime-n
    (kbd "<f5>")  'ghci)

  (evil-define-key 'visual python-ts-mode-map
    (kbd "SPC r") 'slime-v)
  (evil-define-key 'normal python-ts-mode-map
    (kbd "SPC r") 'slime-n
    (kbd "<f5>")  'python-repl)

  (evil-define-key 'normal idris-mode-map
    (kbd "SPC i") 'indent-according-to-mode
    (kbd "SPC I") 'save-and-stylish-hindent-buffer)
  (evil-define-key 'visual idris-mode-map
    (kbd "SPC i") 'indent-region
    (kbd "SPC I") 'save-and-stylish-hindent-buffer)
  (evil-define-key '(normal visual) idris-mode-map
    (kbd "SPC r")   'idris-load-file
    (kbd "SPC f")   'idris-type-at-point
    (kbd "SPC g")   'idris-goto-location
    (kbd "SPC a r") 'idris-refine
    (kbd "SPC a p") 'idris-proof-search
    (kbd "SPC a t") 'idris-type-search
    (kbd "SPC a c") 'idris-case-dwim
    (kbd "SPC a l") 'idris-make-lemma
    (kbd "SPC a m") 'idris-add-missing
    (kbd "SPC a g") 'idris-generate-def
    (kbd "SPC h")   'toggle-hole
    (kbd "SPC M-n") 'idris-next-error
    (kbd "SPC M-p") 'idris-previous-error)

  (evil-define-key 'normal agda2-mode-map
    (kbd "SPC i") 'indent-according-to-mode)
  (evil-define-key 'visual agda2-mode-map
    (kbd "SPC i") 'indent-region)
  (evil-define-key '(normal visual) agda2-mode-map
    (kbd "SPC r")   'agda2-load
    (kbd "SPC R")   'agda2-compile
    (kbd "SPC f")   'agda2-infer-type-maybe-toplevel
    (kbd "SPC g")   'agda2-goto-definition-keyboard
    (kbd "SPC a r") 'agda2-refine
    (kbd "SPC a s") 'agda2-solve-maybe-all
    (kbd "SPC a a") 'agda2-mimer-maybe-all
    (kbd "SPC a c") 'agda2-make-case
    (kbd "SPC a e") 'agda2-compute-normalised-maybe-toplevel
    (kbd "SPC a g") 'agda2-give
    (kbd "SPC a ,") 'agda2-goal-and-context
    (kbd "SPC M-n") 'agda2-next-goal
    (kbd "SPC M-p") 'agda2-previous-goal)

  (evil-define-key 'normal rustic-mode-map
    (kbd "SPC I") 'rustic-format-buffer)
  (evil-define-key 'visual rustic-mode-map
    (kbd "SPC I") 'rustic-format-region)
  (evil-define-key '(normal visual) rustic-mode-map
    (kbd "SPC r")  'rustic-cargo-run
    (kbd "<f4>")   'rustic-popup
    (kbd "<f5>")   'rust-compile-and-dap
    (kbd "C-<f5>") 'rustic-cargo-build
    (kbd "M-<f5>") 'rustic-cargo-test)

  (evil-define-key 'normal conf-toml-mode-map
    (kbd "<f5>")   'rust-compile-and-dap
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

  (evil-define-key 'normal bash-ts-mode-map
    (kbd "SPC r") (lambda () (interactive)
                    (save-buffer)
                    (shell-command (concat "bash" (buffer-file-name)))
                    (revert-buffer t t t)))
  (evil-define-key 'visual bash-ts-mode-map
    (kbd "SPC r") (lambda () (interactive)
                    (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))))
  (evil-define-key 'normal sh-mode-map
    (kbd "SPC r") (lambda () (interactive)
                    (save-buffer)
                    (shell-command (concat "bash" (buffer-file-name)))
                    (revert-buffer t t t)))
  (evil-define-key 'visual sh-mode-map
    (kbd "SPC r") (lambda () (interactive)
                    (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))))

  (evil-define-key 'normal c-mode-map
    (kbd "I")    'save-and-clang-format-buffer
    (kbd "<f5>") 'gdb)
  (evil-define-key 'normal c++-mode-map
    (kbd "I")    'save-and-clang-format-buffer
    (kbd "<f5>") 'gdb)
  (evil-define-key 'normal c-ts-mode-map
    (kbd "I")    'save-and-clang-format-buffer
    (kbd "<f5>") 'gdb)
  (evil-define-key 'normal c++-ts-mode-map
    (kbd "I")    'save-and-clang-format-buffer
    (kbd "<f5>") 'gdb)

  (evil-define-key 'visual cpp2-mode-map
    (kbd "SPC i") 'indent-region)
  (evil-define-key 'normal cpp2-mode-map
    (kbd "SPC i") 'indent-according-to-mode
    (kbd "SPC I") 'my/indent-buffer)
  (evil-define-key '(normal visual) cpp2-mode-map
    (kbd "SPC v")  'gud-break)

  (evil-define-key 'normal java-ts-mode-map
    (kbd "I") 'save-and-clang-format-buffer
    (kbd "<f5>") 'jdb)

  (evil-define-key 'normal kotlin-mode-map
    (kbd "<f5>") 'jdb)

  (evil-define-key 'normal tuareg-mode-map
    (kbd "SPC I") 'ocamlformat)

  (evil-define-key 'normal swift-ts-mode-map
    (kbd "SPC I") 'save-and-swift-format-buffer)

  (evil-define-key 'normal dyalog-mode-map
    (kbd "SPC d") 'my/toggle-display)
  (evil-define-key '(insert replace) dyalog-mode-map
    (kbd "C-a")   apl-keymap)

  (evil-define-key 'normal gud-mode-map
    (kbd "SPC d") 'gud-step
    (kbd "SPC s") 'gud-next
    (kbd "SPC a") 'gud-stepi
    (kbd "SPC e") 'gud-cont
    (kbd "SPC w") 'gud-finish)

  (evil-define-key 'insert vterm-mode-map
    (kbd "C-V")      'vterm-yank
    (kbd "<delete>") 'vterm-send-delete
    (kbd "C-g")      'vterm--self-insert
    (kbd "C-a")      'vterm--self-insert
    (kbd "C-d")      'vterm--self-insert
    [?\t]            'vterm--self-insert
    (kbd "<tab>")    'vterm--self-insert)
  (evil-define-key '(normal emacs) vterm-mode-map
    (kbd "C-M-w") 'windmove-up
    (kbd "C-M-s") 'windmove-down
    (kbd "C-M-a") 'windmove-left
    (kbd "C-M-d") 'windmove-right
    (kbd "C-V")   'vterm-yank
    (kbd "C-v")   'vterm-yank)
  (evil-define-key 'normal vterm-mode-map
    (kbd "SPC r") 'vt-reload)

  (evil-define-key 'motion compilation-mode-map
    (kbd "SPC m") 'magit
    (kbd "SPC v") 'hs-toggle-hiding
    (kbd "ö")     'evil-ex
    (kbd "Ö")     'eval-expression
    (kbd "SPC c") 'kill-compilation)
  (evil-define-key 'visual compilation-mode-map
    (kbd "SPC r") 'enable-ansi-in-shell-output)

  (evil-define-key 'normal treemacs-mode-map
    (kbd "SPC")      'treemacs-TAB-action
    (kbd "<tab>")    'treemacs-TAB-action
    [?\t]            'treemacs-TAB-action
    (kbd "C-<tab>")  'treemacs-switch-workspace
    (kbd "x")        'treemacs-delete-file
    (kbd "<delete>") 'treemacs-delete-file
    (kbd "a")        nil
    (kbd "d")        nil
    (kbd "q")        'evil-quit)

  (evil-define-key '(normal emacs motion) Buffer-menu-mode-map
    (kbd "<return>") 'Buffer-menu-this-window
    (kbd "C-n")      'nuke-all-buffers)

  (evil-define-key 'normal pdf-view-mode-map
    (kbd "SPC g") 'pdf-view-goto-page
    (kbd "SPC G") 'pdf-view-goto-label
    (kbd "w")     'pdf-view-previous-page-command
    (kbd "a")     'pdf-view-previous-page-command
    (kbd "s")     'pdf-view-next-page-command
    (kbd "d")     'pdf-view-next-page-command)

  (evil-define-key 'normal image-mode-map
    (kbd "C-+") 'image-increase-size
    (kbd "C--") 'image-decrease-size)

  (evil-define-key 'normal forge-issue-mode-map
    (kbd "i")     'forge-create-post
    (kbd "F")     'forge-pull-this-topic
    (kbd "<tab>") 'magit-section-toggle
    (kbd "q")     'magit-mode-bury-buffer))

(provide 'qwerty)
;;; qwerty.el ends here
