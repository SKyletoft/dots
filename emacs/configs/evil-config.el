;;; evil-config.el --- A bunch of functions I use interactively -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/is-whitespace (c)
  "Returns true if C is \\t, \\r, \\n, \\s, \\e or \\a."
  (or (eq c ?\t)
      (eq c ?\r)
      (eq c ?\n)
      (eq c ?\s)
      (eq c ?\e)
      (eq c ?\a)))

(defun my/snippet-complete-or-indent ()
  "Try to complete with Yasnippet, fallback to company, and indent if both fail."
  (interactive)
  (cond
   ((yas-expand) t)
   ((my/is-whitespace (char-before)) (my/indent-line))
   ((call-interactively 'company-complete-selection) t)))

(defun my/snippet-or-indent ()
  "Try to complete with Yasnippet and indent if it fails."
  (interactive)
  (cond
   ((yas-expand) t)
   ((my/is-whitespace (char-before)) (my/indent-line))))

;; Indents
(defun my/indent-line ()
  (interactive)
  (evil-shift-right-line 1))

(defun my/outdent-line ()
  (interactive)
  (evil-shift-left-line 1))

(defun my/indent-region ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end) 1)
  (evil-force-normal-state)
  (evil-visual-restore))

(defun my/outdent-region ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end) 1)
  (evil-force-normal-state)
  (evil-visual-restore))

(setq backward-delete-char-untabify-method 'hungry)
(setq-default evil-shift-width tab-width)
(setq align-to-tab-stop nil)

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
  "Remove closest wrapping (), [] or {}."
  (interactive)
  (save-mark-and-excursion
    (return-to-open-brace)
    (save-mark-and-excursion
      (evil-jump-item)
      (delete-forward-char 1))
    (delete-forward-char 1)))

(defun comment-or-uncomment-line ()
  "Mark current line as region and run \"comment-or-uncomment-region\"."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

;; Pane management
(defun transpose-with-treemacs ()
  "Transpose the frame but without breaking treemacs."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (treemacs)
              (transpose-frame)
              (treemacs))
    ('exists (transpose-frame))
    ('none (transpose-frame))))

(defun create-sidebar (len)
  "Create a LEN long string of dashes."
  (if (<= len 1) ;; At least one - or it just looks dumb
      "-"
    (concat "-" (create-sidebar (- len 1)))))

;; Section header insertion function
(defun insert-header (title width)
  "Insert a - wrapped TITLE with the text centred on a WIDTH long line:
----- EXAMPLE -----"
  (interactive
   (list
    (read-string "Title: ")
    (read-number "Width: " fill-column)))
  (let* ((sidebar (create-sidebar
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
      (if (eq (length (get-buffer-window-list))
              1)
          (kill-buffer-and-window)
        (evil-window-delete))))

;; Tree-sitter textobjs
(use-package evil-textobj-tree-sitter)

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

(defun select-with-symex ()
  (interactive)
  (evil-visual-state)
  (symex-select-nearest))

(setq my/left-margin-p nil)
(setq-default my/left-margin-default 85)

(defun my/set-left-margin (to)
  "Sets how wide the indent of \"my/toggle-left-margin\" should be (= TO)."
  (interactive "nTo: ")
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil nil)) to))

(defun my/toggle-left-margin ()
  (interactive)
  (my/set-left-margin (if my/left-margin-p
                       0
                     my/left-margin-default))
  (setq-local my/left-margin-p (not my/left-margin-p)))

(defun get-buffer-list ()
  "Switches to the buffer list in the current window.
As opposed to `list-buffers` which will split the window."
  (interactive)
  (switch-to-buffer "*Buffer List*")
  (list-buffers)
  (evil-motion-state))

(defun save-and-black-buffer ()
  (interactive)
  (save-buffer)
  (shell-command (concat "black "
                         (buffer-file-name)))
  (revert-buffer t t t))

(defun save-and-clang-format-buffer ()
  (interactive)
  (save-buffer)
  (shell-command (concat "clang-format -i "
                         (buffer-file-name)))
  (revert-buffer t t t))

(defun save-and-swift-format-buffer ()
  (interactive)
  (save-buffer)
  (shell-command (concat "swift-format -i "
                         (buffer-file-name)))
  (revert-buffer t t t))

(defun save-and-stylish-hindent-buffer ()
  (interactive)
  (save-buffer)
  (shell-command (concat "hindent "
                         (buffer-file-name)
                         " && stylish-haskell -i "
                         (buffer-file-name)))
  (revert-buffer t t t))

(require 'ansi-color)
(defun enable-ansi-in-shell-output ()
  "Switch to shell output and enable ansi colours."
  (interactive)
  (shell-command (string-trim (buffer-substring (region-beginning) (region-end))))
  (switch-to-buffer-other-window "*Shell Command Output*")
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun my/toggle-display ()
"Toggle whether or not line starts with ]DISPLAY.  Useful for Dyalog APL."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (string= "]DISPLAY "
                 (buffer-substring (point) (+ (point) 9)))
        (delete-char 9)
      (insert "]DISPLAY "))))

(setq my/last-command-buffer "")
(defun run-command (command)
"A second compilation buffer that remembers the COMMAND seperately from 'compile-command'."
  (interactive "MShell command: ")
  (let ((old-compile-command compile-command))
    (compile command)
    (setq-local compile-command old-compile-command
                my/last-command-buffer command)))

(defun up-five ()
  (interactive)
  (evil-scroll-line-up 5))

(defun down-five ()
  (interactive)
  (evil-scroll-line-down 5))

(provide 'evil-config)
