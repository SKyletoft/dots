;;; aesthetics.el --- Theme loading -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions

(defun hide-menu ()
  (interactive)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (or (string-match "X11" system-configuration-features)
            (string-match "PGTK" system-configuration-features))
    (scroll-bar-mode -1)))

(defun show-menu ()
  (interactive)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (when (or (string-match "X11" system-configuration-features)
            (string-match "PGTK" system-configuration-features))
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
  (add-to-list 'default-frame-alist '(font . "Cascadia Code NF-12"))
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
  (set-face-attribute 'default nil :font "Cascadia Code NF" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "Cascadia Code")
  (set-face-attribute 'variable-pitch nil :font "TeX Gyre Heros" :height 150))

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

;; Agda / math glyph fallbacks.
;; Julia Mono covers the full Mathematical Alphanumeric Symbols block plus most
;; operator/symbol blocks at monospace metrics, so Emacs never reaches for
;; mismatched fonts (e.g. DejaVu Math TeX Gyre, which renders ~2x too large).
;; Higher-priority (later 'prepend) entries override the wider blocks above.
(defun ap--agda-fontset ()
  (dolist (spec '(;; Julia Mono: math alphanumerics, operators, arrows, Latin/Greek extras
                  ((#x2100 . #x22FF) . "Julia Mono")
                  ((#x2300 . #x2BFF) . "Julia Mono")
                  ((#x2E00 . #x2E7F) . "Julia Mono")
                  ((#x20A0 . #x20BF) . "Julia Mono")
                  ((#x1D00 . #x1DFF) . "Julia Mono")
                  ((#x1D400 . #x1D7FF) . "Julia Mono")
                  ((#x1D12A . #x1D12B) . "Julia Mono")
                  ((#x1F100 . #x1F1FF) . "Julia Mono")
                  ((#x0100 . #x024F) . "Julia Mono")
                  ((#x0250 . #x02FF) . "Julia Mono")
                  ((#x1E00 . #x1EFF) . "Julia Mono")
                  ((#x2C60 . #x2C7F) . "Julia Mono")
                  ((#xA700 . #xA7FF) . "Julia Mono")
                  ((#xAB30 . #xAB7F) . "Julia Mono")
                  ((#x204C . #x204F) . "Julia Mono")
                  ((#x2001 . #x2001) . "Julia Mono")
                  ((#x2004 . #x2005) . "Julia Mono")
                  ((#x2016 . #x2016) . "Julia Mono")
                  ((#x2023 . #x2023) . "Julia Mono")
                  ((#x2031 . #x2031) . "Julia Mono")
                  ((#x2034 . #x2037) . "Julia Mono")
                  ((#x203B . #x203B) . "Julia Mono")
                  ((#x203D . #x203D) . "Julia Mono")
                  ((#x2042 . #x2042) . "Julia Mono")
                  ((#x2045 . #x2049) . "Julia Mono")
                  ((#x2051 . #x2052) . "Julia Mono")
                  ((#x2057 . #x2057) . "Julia Mono")
                  ((#x2071 . #x2071) . "Julia Mono")
                  ((#x207A . #x209C) . "Julia Mono")
                  ((#x10140 . #x107BF) . "Julia Mono")
                  ;; Iosevka: the two characters Cascadia and Julia both lack
                  ((#x00AD . #x00AD) . "Iosevka Nerd Font Mono")
                  ((#x20C0 . #x20C0) . "Iosevka Nerd Font Mono")
                  ;; Unifont: CJK punctuation, fullwidth forms, Ethiopian, misc
                  ((#x3000 . #x303F) . "Unifont")
                  ((#xFE00 . #xFE6F) . "Unifont")
                  ((#xFF00 . #xFFEF) . "Unifont")
                  ((#x1364 . #x1366) . "Unifont")
                  ((#x3200 . #x32FF) . "Unifont")
                  ((#xA6F6 . #xA6F6) . "Unifont")
                  ((#xFDFC . #xFDFC) . "Unifont")
                  ;; Unifont Upper: the three glyphs no regular font has
                  ((#x1018E . #x1018E) . "Unifont Upper")
                  ((#x1F6A7 . #x1F6D1) . "Unifont Upper")))
    (set-fontset-font t (car spec) (cdr spec) nil 'prepend)))

(ap--agda-fontset)
(add-hook 'agda2-mode-hook #'ap--agda-fontset)

;; Run setup

;; (on-after-init)
(modus-operandi-load)

(provide 'aesthetics)
;;; aesthetics.el ends here
