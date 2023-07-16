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
        (nuke-all-buffers)
      ())))

;; Variables

(setq after-delete-frame-functions (cons 'kill-buffers-on-all-frames-exited after-delete-frame-functions)
      visual-line-mode -1
      global-auto-revert-mode 1
      hs-minor-mode 1
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
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

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

;; (use-package gcmh
  ;; :config
  ;; (setq garbage-collection-messages t
        ;; gcmh-idle-delay 10
        ;; gcmh-high-cons-threshold (* 1024 1024 128))
  ;; (gcmh-mode 1))

(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))

(require 'emacs-gc-stats)
(setq emacs-gc-stats-gc-defaults 'emacs-defaults) ; optional
(emacs-gc-stats-mode +1)
