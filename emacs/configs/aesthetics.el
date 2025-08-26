;;; aesthetics.el --- Theme loading -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions

(defun hide-menu ()
  (interactive)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (string-match "X11" system-configuration-features)
    (scroll-bar-mode -1)))

(defun show-menu ()
  (interactive)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (when (string-match "X11" system-configuration-features)
    (scroll-bar-mode nil)))

(defun monokai-purple ()
  (interactive)
  (disable-theme 'modus-operandi)
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  (setq-default line-spacing 0.1)

  (setq fill-column 80)
  (add-to-list 'default-frame-alist '(font . "Cascadia Code NF-12"))
  ;; (when (not (or (string-match "PGTK" system-configuration-features)
  ;;                (string-match "wayland" (getenv "XDG_SESSION_TYPE"))))
  ;;   (set-frame-parameter (selected-frame)
  ;;                        'alpha
  ;;                        '(90 . 90))
  ;;   (add-to-list 'default-frame-alist
  ;;                '(alpha . (90 . 90))))

  (load-theme 'custom-monokai t)
  ;; (telephone-line-mode 1)
  (doom-modeline-mode 1)
  (setq-default truncate-lines t)
  (hide-menu))

(defun modus-operandi-load ()
  (interactive)
  (disable-theme 'custom-monokai)
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  (setq-default line-spacing 0.1)

  (setq fill-column 80)
  (if (string-equal (system-name) "medea")
      (add-to-list 'default-frame-alist '(font . "Cascadia Code NF-25"))
    (add-to-list 'default-frame-alist '(font . "Cascadia Code NF-12")))
  (load-theme 'modus-operandi t)
  (doom-modeline-mode 1)
  ;; (telephone-line-mode 0)
  (setq-default truncate-lines t)
  (hide-menu))

;; Disable background when in terminal
(defun on-after-init ()
  "Disable background when in terminal."
  (interactive)
  (if (not (display-graphic-p (selected-frame)))
      (progn (monokai-purple)
             (set-face-background 'default "unspecified-bg" (selected-frame)))
    (modus-operandi-load)))

;; (add-hook 'window-setup-hook 'on-after-init)
;; (add-hook 'server-switch-hook 'on-after-init)
;; (add-to-list 'after-make-frame-functions 'on-after-init)

;; Load packages

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (if (string-equal (system-name) "medea")
      ;; 2x gui scale on medea, but no built-in support for scaling this specific font
      (progn
        (set-face-attribute 'default nil :font "Cascadia Code NF" :height 250)
        (set-face-attribute 'fixed-pitch nil :font "Cascadia Code")
        (set-face-attribute 'variable-pitch nil :font "TeX Gyre Heros" :height 300))
    (progn
      (set-face-attribute 'default nil :font "Cascadia Code NF" :height 120)
      (set-face-attribute 'fixed-pitch nil :font "Cascadia Code")
      (set-face-attribute 'variable-pitch nil :font "TeX Gyre Heros" :height 150))))

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|=" "|>" "|-" "{|" "[|"
                                       "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:" "|]"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

;; Run setup


;; (on-after-init)
(modus-operandi-load)

(provide 'aesthetics)
;;; aesthetics.el ends here
