(provide 'aesthetics)

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
  ;; (add-to-list 'default-frame-alist '(font . "Fira Code Nerd Font-11"))
  ;; (add-to-list 'default-frame-alist '(font . "0xProto-11"))
  ;; (add-to-list 'default-frame-alist '(font . "Monaspace Neon-11")) ;; åäö, wait are all comments in italics now?
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  (setq-default line-spacing 0.1)

  (setq fill-column 80)
  ;; (add-to-list 'default-frame-alist '(font . "Cascadia Code-22"))
  (add-to-list 'default-frame-alist '(font . "Cascadia Code NF-11"))
  (when (not (or (string-match "PGTK" system-configuration-features)
                 (string-match "wayland" (getenv "XDG_SESSION_TYPE"))))
    (add-to-list 'default-frame-alist '(font . "Cascadia Code NF-11"))
    (set-frame-parameter (selected-frame)
                         'alpha
                         '(90 . 90))
    (add-to-list 'default-frame-alist
                 '(alpha . (90 . 90))))

  (load-theme 'custom-monokai t)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (telephone-line-mode)
  (setq-default truncate-lines t)
  (hide-menu))

;; Disable background when in terminal
(defun on-after-init ()
  "Disable background when in terminal"
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
(add-hook 'server-switch-hook 'on-after-init)

;; Load packages

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (if (string-equal (system-name) "medea")
      ;; 2x gui scale on medea, but no built-in support for scaling this specific font
      (set-face-attribute 'fringe nil :font "Cascadia Code NF" :height 220))
  (set-face-attribute 'default nil :font "Cascadia Code NF" :height 110)
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

;; Run setup

(monokai-purple)
