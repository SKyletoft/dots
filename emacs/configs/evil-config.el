(provide 'evil-config)

;; These functions can't be used for bindings starting with space
(defun set-nvm (key command)
  "Set a keybinding in normal, visual and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-visual-state-map key command)
  (define-key evil-motion-state-map key command))

(defun set-nme (key command)
  "Set a keybinding in normal, motion and emacs modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-motion-state-map key command)
  (define-key evil-emacs-state-map key command))

(defun set-nmi (key command)
  "Set a keybinding in normal and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-insert-state-map key command)
  (define-key evil-motion-state-map key command))

(defun set-nm (key command)
  "Set a keybinding in normal and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-motion-state-map key command))

(defun set-ne (key command)
  "Set a keybinding in normal and emacs modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-emacs-state-map key command))

(defun set-nv (key command)
  "Set a keybinding in normal and visual modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-visual-state-map key command))

(defun set-n (key command)
  "Set a keybinding in normal mode"
  (define-key evil-normal-state-map key command))

(defun set-v (key command)
  "Set a keybinding in visual mode"
  (define-key evil-visual-state-map key command))

(defun set-i (key command)
  "Set a keybinding in insert mode"
  (define-key evil-visual-state-map key command))

;; Indents
(defun shift-width-spaces (width)
  "Create a string of spaces that is `width` wide"
  (if (eq width 0)
      ""
    (concat " " (shift-width-spaces (- width 1)))))

(defun insert-tab-at-start ()
  "Insert a tab or `evil-shift-width` spaces (controlled by `indent-tabs-mode`) at
   the beginning of the line and move the cursor accordingly"
  (interactive)
  (let ((pos (point))
        (indent-with (if indent-tabs-mode
                         "\t"
                       (shift-width-spaces evil-shift-width)))
        (step-by (if indent-tabs-mode
                     1
                   evil-shift-width)))
    (beginning-of-line)
    (insert indent-with)
    (goto-char (+ step-by pos))))

(setq backward-delete-char-untabify-method 'hungry)
(setq-default evil-shift-width tab-width)

;; Bracket helpers
(defun wrap-selection-in (start end)
  (save-mark-and-excursion
    (let ((s1 (region-beginning))
          (s2 (region-end)))
      (goto-char s1)
      (insert start)
      (goto-char (+ s2 (length start)))
      (insert end))))

(defun wrap-in-quotes ()
  (interactive)
  (wrap-selection-in "\"" "\""))
(defun wrap-in-apostrophes ()
  (interactive)
  (wrap-selection-in "'" "'"))
(defun wrap-in-ticks ()
  (interactive)
  (wrap-selection-in "`" "`"))
(defun wrap-in-parentheses ()
  (interactive)
  (wrap-selection-in "(" ")"))
(defun wrap-in-curlies ()
  (interactive)
  (wrap-selection-in "{" "}"))
(defun wrap-in-squares ()
  (interactive)
  (wrap-selection-in "[" "]"))
(defun wrap-in-angles ()
  (interactive)
  (wrap-selection-in "<" ">"))
(defun wrap-in-dollar ()
  (interactive)
  (wrap-selection-in "$" "$"))

(defun return-to-open-brace ()
  "Move to previous (, [ or {"
  (let ((open-p 0)
        (open-s 0)
        (open-c 0)
        (cont 't))
    (while cont
      (let ((c (char-after)))
        (cond ((or (and (eq c ?\() (eq open-p 0))
                   (and (eq c ?\[) (eq open-s 0))
                   (and (eq c ?\{) (eq open-c 0))
                   (eq (point) 1))
               (setq cont nil))
              ((eq c ?\))
               (setq open-p (- open-p 1)))
              ((eq c ?\])
               (setq open-s (- open-s 1)))
              ((eq c ?\})
               (setq open-c (- open-c 1)))
              ((eq c ?\()
               (setq open-p (+ open-p 1)))
              ((eq c ?\[)
               (setq open-s (+ open-s 1)))
              ((eq c ?\{)
               (setq open-c (+ open-c 1)))))
      (backward-char))
    (forward-char)))

(defun remove-wrappers ()
  "Remove closest wrapping (), [] or {}"
  (interactive)
  (save-mark-and-excursion
    (return-to-open-brace)
    (save-mark-and-excursion
      (evil-jump-item)
      (delete-forward-char 1))
    (delete-forward-char 1)))

(defun comment-or-uncomment-line ()
  "Mark current line as region and run comment-or-uncomment-region"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

;; Pane management
(defun transpose-with-treemacs ()
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (treemacs)
              (transpose-frame)
              (treemacs))
    ('exists (transpose-frame))
    ('none (transpose-frame))))

(defun create-sidebar (len)
  (if (<= len 1) ;; At least one - or it just looks dumb
      "-"
    (concat "-" (create-sidebar (- len 1)))))

;; Section header insertion function
(defun insert-header ()
  "Insert a - wrapped title with the text centred
----- EXAMPLE -----
"
  (interactive)
  ;; These are lambdas for scoping reasons
  (let* ((title (read-from-minibuffer "Enter title: "))
         (width (string-to-number
                 (read-from-minibuffer "Enter buffer-width: "
                                       (number-to-string fill-column))))
         (sidebar (create-sidebar
                   (/ (- width (+ 2 (length title))) 2)))
         (header (concat sidebar
                         " "
                         title
                         " "
                         sidebar)))
    (insert header)))

(use-package evil)
(setq evil-disable-insert-state-bindings t
      evil-emacs-state-cursor evil-insert-state-cursor)
(evil-set-undo-system 'undo-redo)
(evil-mode 1)

;; Evil quit
(evil-define-command evil-quit
  (&optional force)
  :repeat nil
  :repeat nil
  (if (eq (window-main-window) (selected-window))
      (if (equal (buffer-name) "*dashboard*") nil (kill-buffer))
    (evil-window-delete)))

(defun evil-esc ()
  "Return to symex state if in a mode supported by symex, otherwise return to normal mode"
  (interactive)
  (if (member major-mode '(emacs-lisp-mode
                           rust-ts-mode
                           c-ts-mode
                           c++-ts-mode))
      (symex-mode-interface)
    (evil-force-normal-state)))

;; Tree-sitter textobjs
(use-package evil-textobj-tree-sitter)
(define-key evil-operator-state-map "o" evil-outer-text-objects-map)
(define-key evil-visual-state-map "o" evil-outer-text-objects-map)

(define-key evil-inner-text-objects-map "e" (evil-textobj-tree-sitter-get-textobj ("try_expr"
                                                                                   "unary_expr"
                                                                                   "binary_expr"
                                                                                   "call_expr"
                                                                                   "field_expr"
                                                                                   "identifier"
                                                                                   "scoped_identifier"
                                                                                   "reference_expr"
                                                                                   "struct_expr"
                                                                                   "tuple_expr"
                                                                                   "integer_literal"
                                                                                   "float_literal"
                                                                                   "boolean_literal")
                                              '((rustic-mode . [(try_expression) @try_expr
                                                                (unary_expression) @unary_expr
                                                                (binary_expression) @binary_expr
                                                                (call_expression) @call_expr
                                                                (field_expression) @field_expr
                                                                (identifier) @identifier
                                                                (scoped_identifier) @scoped_identifier
                                                                (reference_expression) @reference_expr
                                                                (struct_expression) @struct_expr
                                                                (tuple_expression) @tuple_expr
                                                                (integer_literal) @integer_literal
                                                                (float_literal) @float_literal
                                                                (boolean_literal) @boolean_literal]))))
(define-key evil-outer-text-objects-map "e" (evil-textobj-tree-sitter-get-textobj ("try_expr"
                                                                                   "unary_expr"
                                                                                   "binary_expr"
                                                                                   "call_expr"
                                                                                   "field_expr"
                                                                                   "identifier"
                                                                                   "scoped_identifier"
                                                                                   "reference_expr"
                                                                                   "struct_expr"
                                                                                   "tuple_expr"
                                                                                   "integer_literal"
                                                                                   "float_literal"
                                                                                   "boolean_literal")
                                              '((rustic-mode . [(try_expression) @try_expr
                                                                (unary_expression) @unary_expr
                                                                (binary_expression) @binary_expr
                                                                (call_expression) @call_expr
                                                                (field_expression) @field_expr
                                                                (identifier) @identifier
                                                                (scoped_identifier) @scoped_identifier
                                                                (reference_expression) @reference_expr
                                                                (struct_expression) @struct_expr
                                                                (tuple_expression) @tuple_expr
                                                                (integer_literal) @integer_literal
                                                                (float_literal) @float_literal
                                                                (boolean_literal) @boolean_literal]))))

(define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                              '((python-mode . [(import_statement) @import])
                                                (rustic-mode . [(use_declaration) @import]))))

(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))

(define-key evil-inner-text-objects-map "e" (evil-textobj-tree-sitter-get-textobj "expression.inner"))
(define-key evil-outer-text-objects-map "e" (evil-textobj-tree-sitter-get-textobj "expression.outer"))

(define-key evil-inner-text-objects-map "s" (evil-textobj-tree-sitter-get-textobj "statement.inner"))
(define-key evil-outer-text-objects-map "s" (evil-textobj-tree-sitter-get-textobj "statement.outer"))

(define-key evil-inner-text-objects-map "b" (evil-textobj-tree-sitter-get-textobj "block.inner"))
(define-key evil-outer-text-objects-map "b" (evil-textobj-tree-sitter-get-textobj "block.outer"))

(define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
(define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))

(define-key evil-inner-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
(define-key evil-outer-text-objects-map "k" (evil-textobj-tree-sitter-get-textobj "comment.outer"))

(define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

(define-key evil-normal-state-map
            (kbd "]f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer")))
(define-key evil-normal-state-map
            (kbd "[f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
(define-key evil-normal-state-map
            (kbd "]F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
(define-key evil-normal-state-map
            (kbd "[F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))

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
  )

(defun colemak-keymap ()
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
  )

(if (eq (system-name) "medusa")
    (colemak-keymap)
  (qwerty-keymap))
