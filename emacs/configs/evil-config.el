(provide 'evil-config)

(defun set-nvm (key command)
  "Set a keybinding in normal, visual and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-visual-state-map key command)
  (define-key evil-motion-state-map key command))

(defun set-nme (key command)
  "Set a keybinding in normal, motion and emacs modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-motion-state-map key command)
  (define-key evil-emacs-state-map key command)

(defun set-nm (key command)
  "Set a keybinding in normal and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-motion-state-map key command))

(defun set-ne (key command)
  "Set a keybinding in normal and emacs modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-emacs-state-map key command))

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
(define-key evil-normal-state-map (kbd "C-t") 'multi-vterm)
(define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "SPC m") 'magit)
(define-key evil-normal-state-map (kbd "SPC n") 'magit-blame)
(define-key evil-normal-state-map (kbd "SPC r") 'eval-buffer)
(define-key evil-visual-state-map (kbd "SPC r") 'eval-region)

;; Evil quit
(evil-define-command evil-quit
  (&optional force)
  :repeat nil
  :repeat nil
  (if (eq (window-main-window) (selected-window))
      (if (equal (buffer-name) "*dashboard*") nil (kill-buffer))
      (evil-window-delete)))

(defun evil-normal-visual-motion (key command)
  "Set a keybinding in normal, visual and motion modes at once"
  (define-key evil-normal-state-map key command)
  (define-key evil-visual-state-map key command)
  (define-key evil-motion-state-map key command))

(defun normal-emacs (key command)
  (define-key evil-normal-state-map key command)
  (define-key evil-emacs-state-map key command))

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
(define-key evil-normal-state-map (kbd "C-c") 'evil-yank)
(define-key evil-normal-state-map (kbd "C-v") 'evil-paste-after)
(define-key evil-normal-state-map (kbd "C-x") 'evil-delete)
(define-key evil-normal-state-map (kbd "M-V") 'evil-visual-block)

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
(evil-normal-visual-motion (kbd "<tab>") (kbd ">>")) ; For gui
(evil-normal-visual-motion [?\t] (kbd ">>")) ; For terminal use
(evil-normal-visual-motion (kbd "<backtab>") (kbd "<<"))
(evil-normal-visual-motion (kbd "<iso-lefttab>") (kbd "<<"))
(define-key evil-visual-state-map (kbd "<tab>") (kbd ">gv"))
(define-key evil-visual-state-map (kbd "<backtab>") (kbd "<gv"))
(define-key evil-visual-state-map (kbd "<iso-lefttab>") (kbd "<gv"))

(setq-default evil-shift-width tab-width)
(setq backward-delete-char-untabify-method 'hungry)

;; Find
(define-key evil-normal-state-map (kbd "C-f") 'evil-search-forward)
(define-key evil-normal-state-map (kbd "C-g") (kbd ":%s/"))

;; Undo and redo
(define-key evil-normal-state-map (kbd "C-z") 'evil-undo)
(define-key evil-normal-state-map (kbd "C-r") 'evil-redo)

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
(defun transpose-with-treemacs ()
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (treemacs)
              (transpose-frame)
              (treemacs))
    ('exists (transpose-frame))
    ('none (transpose-frame))))

(normal-emacs (kbd "C-e") 'split-window-horizontally)
(normal-emacs (kbd "C-q") 'split-window-vertically)
(normal-emacs (kbd "C-M-w") 'evil-window-up)
(normal-emacs (kbd "C-M-s") 'evil-window-down)
(normal-emacs (kbd "C-M-a") 'evil-window-left)
(normal-emacs (kbd "C-M-d") 'evil-window-right)
(normal-emacs (kbd "M-W") 'evil-window-decrease-height)
(normal-emacs (kbd "M-S") 'evil-window-increase-height)
(normal-emacs (kbd "M-D") 'evil-window-increase-width)
(normal-emacs (kbd "M-A") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "SPC o") 'transpose-with-treemacs) ;; SPC prefix breaks typing in Emacs mode

(define-key evil-normal-state-map (kbd "C-<tab>") 'next-buffer)
(define-key evil-motion-state-map (kbd "C-<tab>") 'next-buffer)
(define-key evil-emacs-state-map (kbd "C-<tab>") 'next-buffer)

(define-key evil-normal-state-map (kbd "C-<backtab>") 'previous-buffer)
(define-key evil-motion-state-map (kbd "C-<backtab>") 'previous-buffer)
(define-key evil-emacs-state-map (kbd "C-<backtab>") 'previous-buffer)
(define-key evil-normal-state-map (kbd "C-<iso-lefttab>") 'previous-buffer)
(define-key evil-motion-state-map (kbd "C-<iso-lefttab>") 'previous-buffer)
(define-key evil-emacs-state-map (kbd "C-<iso-lefttab>") 'previous-buffer)

(define-key evil-normal-state-map (kbd "SPC <tab>") 'list-buffers)

;; Formatters

;; Completion-buffer
;; Up/down when completing in the minibuffer
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)
