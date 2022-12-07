(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/")
             t)
(package-initialize)
(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/custom-themes")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("21d3e29ae6afe7556adc1ab149f468cb2d8b27b60e7bd9524b4aeea9b048deca" "06f60c9237f0cfbfdf7ce0f9d3e530b6127f54527abcd21116778909fccf55d9" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" default))
 '(package-selected-packages '(monokai-pro-theme))
 '(warning-suppress-log-types '((comp)))
 )

(defun hide-menu ()
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       (menu-bar-mode -1))

(defun show-menu ()
       (tool-bar-mode nil)
       (scroll-bar-mode nil)
       (menu-bar-mode nil))

(recentf-mode 1)
(setq scroll-step 1)
(setq ring-bell-function 'ignore)
(hide-menu)

(use-package direnv
	:config
	(direnv-mode)
	;; (setq direnv-always-show-summary nil)
	)

(add-to-list 'load-path "~/.emacs.d/configs")
(require 'evil-config)
(require 'aesthetics)
(require 'language-specifics)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
