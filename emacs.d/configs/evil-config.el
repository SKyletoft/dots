(provide 'evil-config)

(use-package evil)
(evil-mode 1)

(global-set-key (kbd "C-M-p") 'evil-force-normal-state)

(defun evil-normal-visual-motion (key command)
  (define-key evil-normal-state-map key command)
  (define-key evil-visual-state-map key command)
  (define-key evil-motion-state-map key command)
  )

;; Basic controls
(evil-normal-visual-motion "a" 'evil-backward-char)
(evil-normal-visual-motion "d" 'evil-forward-char)
(evil-normal-visual-motion "w" 'evil-previous-visual-line)
(evil-normal-visual-motion "s" 'evil-next-visual-line)

;; Larger movements
(evil-normal-visual-motion "\C-a" 'evil-backward-word-begin)
(evil-normal-visual-motion "\C-d" 'evil-forward-word-begin)
(evil-normal-visual-motion "\C-w" #'(lambda () (interactive) (evil-scroll-line-up 5)))
(evil-normal-visual-motion "\C-s" #'(lambda () (interactive) (evil-scroll-line-down 5)))

;; Copy, cut, paste for normal people
(define-key evil-normal-state-map "\C-c" 'evil-yank)
(define-key evil-normal-state-map "\C-v" 'evil-paste-after)
(define-key evil-normal-state-map "\C-x" 'evil-delete)

;; Indents
(setq backward-delete-char-untabify-method 'hungry)
(define-key evil-normal-state-map (kbd "<tab>") #'(lambda () (interactive) (evil-shift-right 0 0)
                                                                           (evil-shift-right 0 0)))
(define-key evil-motion-state-map (kbd "<tab>") #'(lambda () (interactive) (evil-shift-right 0 0)
						                           (evil-shift-right 0 0)))
(define-key evil-normal-state-map (kbd "<backtab>") #'(lambda () (interactive) (evil-shift-left 0 0)
							                       (evil-shift-left 0 0)))
(define-key evil-motion-state-map (kbd "<backtab>") #'(lambda () (interactive) (evil-shift-left 0 0)
									       (evil-shift-left 0 0)))
(setq-default evil-shift-width tab-width)
(setq backward-delete-char-untabify-method 'hungry)

;; Find
(define-key evil-normal-state-map "\C-f" 'evil-search-forward)
;; (define-key evil-normal-state-map "\C-g" 'evil-)

;; Undo and redo
(define-key evil-normal-state-map "\C-z" 'evil-undo)
(define-key evil-normal-state-map "\C-M-r" 'evil-redo)

;; And preserve wasd behaviour somewhere

;; Open file

;; End of line and start of line inserts
(define-key evil-normal-state-map "A" 'evil-insert-line)
(define-key evil-normal-state-map "D" 'evil-append-line)

;; Delete rest of line
(define-key evil-normal-state-map "X" 'evil-delete-line)

;; Unmap undo because it's poorly placed

;; Pane management
;; (define-prefix-command 'evil-window-map)
(define-key evil-normal-state-map "\C-e" 'split-window-horizontally)
(define-key evil-normal-state-map "\C-q" 'split-window-vertically)
(define-key evil-normal-state-map "\C-\M-w" 'evil-window-up)
(define-key evil-normal-state-map "\C-\M-s" 'evil-window-down)
(define-key evil-normal-state-map "\C-\M-a" 'evil-window-left)
(define-key evil-normal-state-map "\C-\M-d" 'evil-window-right)

;; Formatters

