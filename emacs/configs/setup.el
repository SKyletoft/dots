;;; setup.el --- Stuff that used to live in init.el before I decided to make that just imports  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions

(defun nuke-all-buffers ()
  "Kills all buffers."
  (interactive)
  (about-emacs)
  (mapc (lambda (buf)
            (if (not (string= (buffer-name buf) "*About GNU Emacs*"))
                (kill-buffer buf)))
          (buffer-list))
  (delete-other-windows))

;; Nuke all buffers after 8h of idle time
(run-with-idle-timer (* 60 60 8) t 'nuke-all-buffers)

(defun kill-buffers-on-all-frames-exited (_)
  "Kills all buffers if no frames exist."
  (let ((frames (length (x-frame-list-z-order))))
    (if (eq frames 0)
        (progn
          (nuke-all-buffers)
          (about-emacs)))))

;; Variables

(setq visual-line-mode -1
      frame-resize-pixelwise t
      xterm-mouse-mode 1
      global-auto-revert-non-file-buffers t
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
      initial-scratch-message ""
      auth-sources '("~/.authinfo"))
(add-hook 'prog-mode-hook 'auto-revert-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Load packages

(use-package editorconfig)
(editorconfig-mode 1)

(use-package telephone-line)
(use-package doom-modeline)

(use-package treemacs
  :config
  (setq treemacs-space-between-root-nodes nil))

(use-package direnv
  :config
  (setq direnv-always-show-summary t)
  (direnv-mode))

(use-package yasnippet-snippets
  :defer 2)
(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1))
(add-hook 'snippet-mode-hook (lambda () (remove-hook before-save whitespace-cleanup t)))

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

(use-package tramp
  :config
  (setq tramp-remote-path (append tramp-remote-path
                                  '(tramp-own-remote-path))))

(use-package magit)
(use-package forge
  :after magit)

(use-package nerd-icons)
(use-package all-the-icons)
;; (nerd-icons-install-fonts 't)
;; (all-the-icons-install-fonts 't)

(use-package gptel
  :config
  (setq gptel-model 'mistral:latest
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(mistral:latest))))

;; GDB/GUD-GDB
(setq gdb-debuginfod-enable-setting nil)

(provide 'setup)
;;; setup.el ends here.
