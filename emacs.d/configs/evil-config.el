(provide 'evil-config)

(use-package evil)
(setq evil-disable-insert-state-bindings t)
(evil-mode 1)

;; Emacs stuff
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-M-o") 'evil-force-normal-state)
(define-key evil-emacs-state-map (kbd "C-M-i") 'evil-force-normal-state)
(define-key evil-normal-state-map (kbd "C-M-i") 'evil-emacs-state)
(global-set-key (kbd "C-M-p") 'treemacs)
(define-key evil-normal-state-map "\C-t" 'vterm)
(define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(setq evil-emacs-state-cursor evil-insert-state-cursor)

(defun evil-normal-visual-motion (key command)
  "Set a keybinding in normal, visual and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-visual-state-map key command)
  (define-key evil-motion-state-map key command))

;; Basic controls
(evil-normal-visual-motion (kbd "a") 'evil-backward-char)
(evil-normal-visual-motion (kbd "d") 'evil-forward-char)
(evil-normal-visual-motion (kbd "w") 'evil-previous-visual-line)
(evil-normal-visual-motion (kbd "s") 'evil-next-visual-line)

;; Larger movements
(evil-normal-visual-motion (kbd "C-a") 'evil-backward-word-begin)
(evil-normal-visual-motion (kbd "C-d") 'evil-forward-word-begin)
(evil-normal-visual-motion (kbd "C-w") (lambda () (interactive) (evil-scroll-line-up 5)))
(evil-normal-visual-motion (kbd "C-s") (lambda () (interactive) (evil-scroll-line-down 5)))

(evil-normal-visual-motion "W" "%")
(evil-normal-visual-motion "S" "}")

;; Copy, cut, paste for normal people
(define-key evil-normal-state-map "\C-c" 'evil-yank)
(define-key evil-normal-state-map "\C-v" 'evil-paste-after)
(define-key evil-normal-state-map "\C-x" 'evil-delete)

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

(define-key evil-insert-state-map (kbd "TAB") 'insert-tab-at-start)

(setq backward-delete-char-untabify-method 'hungry)
(evil-normal-visual-motion (kbd "<tab>") (kbd ">>"))
(evil-normal-visual-motion (kbd "<backtab>") (kbd "<<"))
(define-key evil-visual-state-map (kbd "<tab>") (kbd ">gv"))
(define-key evil-visual-state-map (kbd "<backtab>") (kbd "<gv"))

(setq-default evil-shift-width tab-width)
(setq backward-delete-char-untabify-method 'hungry)

;; Find
(define-key evil-normal-state-map (kbd "C-f") 'evil-search-forward)
(define-key evil-normal-state-map (kbd "C-g") (kbd ":%s/"))

;; Undo and redo
(define-key evil-normal-state-map (kbd "C-z") 'evil-undo)
(define-key evil-normal-state-map (kbd "C-M-r") 'evil-redo)

;; And preserve wasd behaviour somewhere

;; Open file
;; (define-key evil-normal-state-map "\C-o" (kbd ":e "))

;; End of line and start of line inserts
(define-key evil-normal-state-map "A" 'evil-insert-line)
(define-key evil-normal-state-map "D" 'evil-append-line)

;; Delete rest of line
(define-key evil-normal-state-map (kbd "X") 'evil-delete-line)

;; Unmap undo because it's poorly placed
(define-key evil-normal-state-map (kbd "u") nil)

;; Pane management
(define-key evil-normal-state-map (kbd "C-e") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "C-q") 'split-window-vertically)
(define-key evil-normal-state-map (kbd "C-M-w") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-M-s") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-M-a") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-M-d") 'evil-window-right)
(define-key evil-emacs-state-map (kbd "C-e") 'split-window-horizontally)
(define-key evil-emacs-state-map (kbd "C-q") 'split-window-vertically)
(define-key evil-emacs-state-map (kbd "C-M-w") 'evil-window-up)
(define-key evil-emacs-state-map (kbd "C-M-s") 'evil-window-down)
(define-key evil-emacs-state-map (kbd "C-M-a") 'evil-window-left)
(define-key evil-emacs-state-map (kbd "C-M-d") 'evil-window-right)

;; Formatters

