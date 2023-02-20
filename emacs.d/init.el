(require 'package)
(package-initialize)
(unless(package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes")

(defun hide-menu ()
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(defun show-menu ()
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode nil))

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

(setq after-delete-frame-functions
      (cons 'kill-buffers-on-all-frames-exited after-delete-frame-functions))

(visual-line-mode -1) ;; Truncate lines
(global-auto-revert-mode 1)
(xterm-mouse-mode 1)
(recentf-mode 1)
(setq global-auto-revert-non-file-buffers t
      recentf-max-menu-items 25
      recentf-max-saved-items 25
      ;; scroll-step 1
      ring-bell-function 'ignore
      use-dialog-box nil)

(hide-menu)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package telephone-line)

(use-package treemacs
  :config
  (setq treemacs-space-between-root-nodes nil))

(use-package lsp-mode
  :config
  (setq read-process-output-max (* 1024 1024))) ;; LSP perf hack

(use-package direnv
  :hook
  ((after-init . direnv-mode)
   (lsp-before-initialize-hook . direnv-update-environment)) ;; Doesn't work?
  :config
  (setq direnv-always-show-summary t))

(use-package gcmh
  :config
  (setq garbage-collection-messages t
        gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 1024 1024 128))
  (gcmh-mode 1))

(add-to-list 'load-path "~/.emacs.d/configs")
(require 'evil-config)
(require 'aesthetics)
(require 'language-specifics)
(require 'org-config)
;; (require 'irc-conf)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq custom-file (locate-user-emacs-file "configs/custom-vars.el"))
(load custom-file 'noerror 'nomessage)
