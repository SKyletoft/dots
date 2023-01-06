(provide 'org-config)

(add-hook 'org-indent-mode-hook
          (lambda ()
            (setq-local org-indent-mode nil)
            (setq-local tab-width 8)
            (setq-local evil-shift-width 8)
            (setq-local indent-tabs-mode t)))

;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  ;;(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
  ;;(set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
  ;;(set-face-attribute 'variable-pitch nil :font "DejaVu Sans")
  )

(add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Nice bullets
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
			     (org-superstar-mode 1))))

;; Increase size of LaTeX fragment previews
(plist-put org-format-latex-options :scale 2)

;; Increase line spacing
;; (setq-default line-spacing 6)

;; Distraction-free screen
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
	(progn
	  (window-configuration-to-register 1)
	  (delete-other-windows)
	  (text-scale-increase 2)
	  (olivetti-mode t))
      (progn
	(jump-to-register 1)
	(olivetti-mode 0)
	(text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))
