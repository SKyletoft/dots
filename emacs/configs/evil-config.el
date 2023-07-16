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

(defun set-v (key command)
  "Set a keybinding in visual mode"
  (define-key evil-visual-state-map key command))

(defun set-i (key command)
  "Set a keybinding in insert mode"
  (define-key evil-visual-state-map key command))

(use-package evil)
(setq evil-disable-insert-state-bindings t
      evil-emacs-state-cursor evil-insert-state-cursor
      evil-undo-system 'undo-redo)
(evil-mode 1)

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
(define-key evil-normal-state-map (kbd "SPC r") 'eval-buffer)
(define-key evil-visual-state-map (kbd "SPC r") 'eval-region)
(define-key evil-normal-state-map (kbd "SPC v") 'hs-toggle-hiding)
(define-key company-active-map (kbd "<tab>") 'company-complete-common 'delete)

;; Evil quit
(evil-define-command evil-quit
  (&optional force)
  :repeat nil
  :repeat nil
  (if (eq (window-main-window) (selected-window))
      (if (equal (buffer-name) "*dashboard*") nil (kill-buffer))
    (evil-window-delete)))

;; Basic controls
(set-nvm (kbd "a") 'evil-backward-char)
(set-nvm (kbd "d") 'evil-forward-char)
(set-nvm (kbd "w") 'evil-previous-visual-line)
(set-nvm (kbd "s") 'evil-next-visual-line)

;; Larger movements
(set-nvm (kbd "C-a") 'evil-backward-word-begin)
(set-nm  (kbd "C-d") 'evil-forward-word-begin)
(set-v   (kbd "C-d") 'evil-forward-word-end)
(set-nvm (kbd "C-w") (lambda () (interactive) (evil-scroll-line-up 5)))
(set-nvm (kbd "C-s") (lambda () (interactive) (evil-scroll-line-down 5)))

(set-nvm "W" "%")
(set-nvm "S" "}")

;; Copy, cut, paste for normal people
(set-nm (kbd "C-c") 'evil-yank)
(set-nm (kbd "C-v") 'evil-paste-after)
(set-nm (kbd "C-x") 'evil-delete)
(set-nm (kbd "M-V") 'evil-visual-block)

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

(set-v (kbd "\"") 'wrap-in-quotes)
(set-v (kbd "'") 'wrap-in-apostrophes)
(set-v (kbd "`") 'wrap-in-ticks)
(set-v (kbd "(") 'wrap-in-parentheses)
(set-v (kbd "{") 'wrap-in-curlies)
(set-v (kbd "[") 'wrap-in-squares)
;; (set-v (kbd "<") 'wrap-in-angles) ; Interferes with outdent

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

;; Find
(set-nm (kbd "C-f") 'evil-search-forward)
(set-nm (kbd "C-g") (kbd ":%s/"))

;; Undo and redo
(set-nm (kbd "C-z") 'evil-undo)
(set-nm (kbd "C-r") 'evil-redo)

;; And preserve wasd behaviour somewhere

;; Open file
;; (define-key evil-normal-state-map "\C-o" (kbd ":e "))

;; End of line and start of line inserts
(set-nm "A" 'evil-insert-line)
(set-nm "D" 'evil-append-line)

;; Delete rest of line
(set-nm (kbd "X") 'evil-delete-line)

;; Unmap undo because it's poorly placed
(set-nm (kbd "u") nil)

;; Pane management
(defun transpose-with-treemacs ()
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (treemacs)
              (transpose-frame)
              (treemacs))
    ('exists (transpose-frame))
    ('none (transpose-frame))))

(set-ne (kbd "C-e") 'split-window-horizontally)
(set-ne (kbd "C-q") 'split-window-vertically)
(set-ne (kbd "C-M-w") 'evil-window-up)
(set-ne (kbd "C-M-s") 'evil-window-down)
(set-ne (kbd "C-M-a") 'evil-window-left)
(set-ne (kbd "C-M-d") 'evil-window-right)
(set-ne (kbd "M-W") 'evil-window-decrease-height)
(set-ne (kbd "M-S") 'evil-window-increase-height)
(set-ne (kbd "M-D") 'evil-window-increase-width)
(set-ne (kbd "M-A") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "SPC o") 'transpose-with-treemacs) ;; SPC prefix breaks typing in Emacs mode

(set-nme (kbd "C-<tab>") 'next-buffer)
(set-nme [C-?\t] 'next-buffer)
(set-nme (kbd "C-<backtab>") 'previous-buffer)
(set-nme (kbd "C-<iso-lefttab>") 'previous-buffer)
(define-key evil-normal-state-map (kbd "SPC <tab>") 'list-buffers)
(define-key evil-visual-state-map (kbd "SPC <tab>") 'list-buffers)

;; Completion-buffer
;; Up/down when completing in the minibuffer
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)
