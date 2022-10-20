(provide 'aesthetics)
(add-to-list 'default-frame-alist '(font . "Cascadia Mono PL Semibold-11"))

(set-frame-parameter (selected-frame)
		     'alpha
		     '(90 . 95))
(add-to-list 'default-frame-alist
	     '(alpha . (90 . 95)))

(load-theme 'custom-monokai t)
(global-display-line-numbers-mode t)

;; Disable background when in terminal
(defun on-after-init ()
       (unless (display-graphic-p (selected-frame))
               (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)
