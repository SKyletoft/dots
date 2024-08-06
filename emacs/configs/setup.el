(provide 'setup)

;; Functions

(defun nuke-all-buffers ()
  "Kills all buffers"
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(defun kill-buffers-on-all-frames-exited (_)
  "Kills all buffers if no frames exist"
  (let ((frames (length (x-frame-list-z-order))))
    (if (eq frames 0)
        (progn
          (nuke-all-buffers)
          (about-emacs)))))

;; Variables

(setq visual-line-mode -1
      global-auto-revert-mode 1
      auto-revert-mode 1
      hs-minor-mode 1
      frame-resize-pixelwise t
      xterm-mouse-mode 1
      recentf-mode 1
      global-auto-revert-non-file-buffers t
      recentf-max-menu-items 25
      recentf-max-saved-items 25
      ring-bell-function 'ignore
      use-dialog-box nil
      show-trailing-whitespace t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(1 ((shift) . 2))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      initial-scratch-message "")

(defalias 'yes-or-no-p 'y-or-n-p)

;; Load packages

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package telephone-line)

(use-package treemacs
  :config
  (setq treemacs-space-between-root-nodes nil))

(use-package direnv
  :hook
  ((after-init . direnv-mode)
   (lsp-before-initialize-hook . direnv-update-environment)) ;; Doesn't work?
  :config
  (setq direnv-always-show-summary t))

(use-package yasnippet-snippets
  :defer 2)
(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1))

(use-package ivy)
(ivy-mode)

(use-package projectile)
(projectile-mode +1)

(use-package ivy-posframe
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))
(ivy-posframe-mode 1)

(use-package whitespace
  :hook (before-save . whitespace-cleanup))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5))
(which-key-mode)

(use-package diff-hl)
(global-diff-hl-mode 1)

(use-package company)

(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (setq-local vterm-term-environment-variable 'eterm-color
                              vterm-kill-buffer-on-exit t
                              vterm-timer-delay nil)
                  (evil-emacs-state))))

(use-package tramp-sh
  :config
  (setq tramp-remote-path (append tramp-remote-path
                                  '(tramp-own-remote-path))))
