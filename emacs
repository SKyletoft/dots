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

(global-auto-revert-mode t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
(hide-menu)

(use-package telephone-line
  :config
  (telephone-line-mode))

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
  (setq garbage-collection-messages t)
  (setq gcmh-idle-delay 10)
  (setq gcmh-high-cons-threshold (* 1024 1024 128))
  (gcmh-mode 1))

(add-to-list 'load-path "~/.emacs.d/configs")
(require 'evil-config)
(require 'aesthetics)
(require 'language-specifics)
;; (require 'irc-conf)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
