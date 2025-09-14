;;; setup.el --- Stuff that used to live in init.el before I decided to make that just imports  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions

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
      auth-sources '("~/.authinfo")
      xref-search-program 'ripgrep)
(add-hook 'prog-mode-hook 'auto-revert-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Load packages

; NOTE: THIS SHOULD BE THE FIRST PACKAGE LOADED
; (use-package compile-angel
  ; :ensure t
  ; :demand t
  ; :config
  ; (setq compile-angel-verbose t)
  ; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
  ; (push "/init.el" compile-angel-excluded-files)
  ; (push "/early-init.el" compile-angel-excluded-files)
  ; (compile-angel-on-load-mode 1))

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes
  (buffer-terminator-interval (* 10 60)) ; 10 minutes
  :config
  (buffer-terminator-mode 1))

(use-package editorconfig)
(editorconfig-mode 1)

(use-package doom-modeline)

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
(add-hook 'snippet-mode-hook (lambda () (remove-hook before-save-hook whitespace-cleanup t)))

(use-package ivy)
(ivy-mode)

(use-package ivy-posframe
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))
(ivy-posframe-mode 1)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location))
  :init
  (global-corfu-mode))

(use-package whitespace
  :hook (before-save . whitespace-cleanup))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5))
(which-key-mode)

(use-package diff-hl)
(global-diff-hl-mode 1)

(use-package vterm
  :config (setenv "BAT_THEME" "ansi")
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
  :after magit evil
  :hook
  (forge-pullreq-mode . (lambda () (evil-emacs-state))))

;; GDB/GUD-GDB
(setq gdb-debuginfod-enable-setting nil)

(use-package ispell
  :config
  (setq flyspell-mode 1
        flyspell-prog-mode 1))
(use-package flyspell
  :config
  (require 'dictionary-path)
  (setq ispell-dictionary "en_GB"))


(provide 'setup)
;;; setup.el ends here.
