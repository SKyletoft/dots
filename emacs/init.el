;;; init.el --- Load the config and set keymaps  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/configs")
(add-to-list 'load-path "~/.emacs.d/configs/languages")
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes")
;; (add-to-list 'load-path "~/.emacs.d/configs/copilot.el")
(put 'narrow-to-region 'disabled nil)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      custom-file (locate-user-emacs-file "configs/custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Set GC super high and then set it to something reasonable after configuration is done
(setq gc-cons-threshold (* 16 1024 1024 1024))

(require 'packages-config)
(require 'setup)
;; (require 'org-config)
(require 'evil-config)
;; (require 'typst)
(require 'aesthetics)
;; (require 'dap)
(require 'artemis-mode)
(require 'jasmin)
(require 'promela-2-mode)
(require 'number-hl-mode)
(require 'slimish)
(require 'language-specifics)
(require 'language-checking)
(require 'lsp-booster)
(require 'qwerty)
(require 'colemak)

;; Would go in setup.el, but needs to be loaded last
(if (string-equal (system-name) "medusa")
    (progn
      (colemak-keymap)
      (setq left-margin-default 80))
  (qwerty-keymap)
  (setq left-margin-default 60))

(use-package gcmh
  :config
  (setq garbage-collection-messages t
        gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 1024 1024 128))
  (gcmh-mode 1))
(unless (version< emacs-version "27.0")
  (add-function :after after-focus-change-function
                (lambda ()
                  (unless (frame-focus-state)
                    (garbage-collect)))))

(provide 'init)
;;; init.el ends here
