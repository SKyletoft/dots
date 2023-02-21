(provide 'aesthetics)

(defun monokai-purple ()
  (add-to-list 'default-frame-alist '(font . "Fira Code Nerd Font-11"))
  (add-to-list 'default-frame-alist '(undecorated-round . t))

  (set-frame-parameter (selected-frame)
                       'alpha
                       '(90 . 90))
  (add-to-list 'default-frame-alist
               '(alpha . (90 . 90)))

  (load-theme 'custom-monokai t)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (telephone-line-mode)
  (setq-default truncate-lines t))

(monokai-purple)

;; Disable background when in terminal
(defun on-after-init ()
       (unless (display-graphic-p (selected-frame))
               (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)
(add-hook 'server-switch-hook 'on-after-init)

;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "Fira Code Nerd Font" :height 110)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font")
  (set-face-attribute 'variable-pitch nil :font "TeX Gyre Heros" :height 150))

;; (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

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
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))
