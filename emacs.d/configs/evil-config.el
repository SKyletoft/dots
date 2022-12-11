(provide 'evil-config)

(use-package evil)
(evil-mode 1)

;; Emacs stuff
(global-set-key (kbd "C-M-p") 'evil-force-normal-state)
(global-set-key (kbd "C-M-å") 'treemacs)
(define-key evil-normal-state-map "\C-t" 'vterm)

(defun evil-normal-visual-motion (key command)
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

;; Copy, cut, paste for normal people
(define-key evil-normal-state-map "\C-c" 'evil-yank)
(define-key evil-normal-state-map "\C-v" 'evil-paste-after)
(define-key evil-normal-state-map "\C-x" 'evil-delete)

;; Indents
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

;; Formatters

