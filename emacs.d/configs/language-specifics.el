(provide 'language-specifics)


(setq-default indent-tabs-mode t)
(setq custom-tab-width 8)
(setq-default evil-shift-width custom-tab-width)
(setq read-process-output-max (* 1024 1024)) ;; LSP perf hack

;; default tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
;; rust    tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
;; fortran tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
;; python  tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
;; nim     tabstop=2 softtabstop=2 shiftwidth=0   expandtab
;; kotlin  tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
;; fut     tabstop=2 softtabstop=2 shiftwidth=0 noexpandtab
;; haskell tabstop=2 softtabstop=2 shiftwidth=0   expandtab
;; fsharp  tabstop=2 softtabstop=2 shiftwidth=0   expandtab
;; nix     tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
;; toml    tabstop=2 softtabstop=2 shiftwidth=0   expandtab
;; yaml    tabstop=2 softtabstop=2 shiftwidth=0   expandtab

(add-hook 'find-file-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                       (string-match "\\.rs\\'" buffer-file-name))
              (setq-local tab-width 8)
              (setq-local evil-shift-width 8)
              (setq-local indent-tabs-mode t))
            (when (and (stringp buffer-file-name)
                       (string-match "\\.nix\\'" buffer-file-name))
              (setq-local tab-width 4)
              (setq-local evil-shift-width 2)
              (setq-local indent-tabs-mode t))
            (when (and (stringp buffer-file-name)
                       (string-match "\\.hs\\'" buffer-file-name))
              (setq-local tab-width 8)
              (setq-local evil-shift-width 2)
              (setq-local indent-tabs-mode nil))
            ))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local indent-tabs-mode nil)))

(use-package eglot)

(add-hook 'vterm-mode-hook
	(lambda ()
		(setq vterm-term-environment-variable 'eterm-color)
		(setq vterm-kill-buffer-on-exit t)
		(define-key vterm-mode-map (kbd "<C-backspace>")
			(lambda () (interactive) (vterm-send-key (kbd "C-w"))))
		(define-key vterm-mode-map (kbd "<C-d>")
			(lambda () (interactive) (vterm-send-key (kbd "C-d"))))
		(define-key vterm-mode-map (kbd "<C-r>")
			(lambda () (interactive) (vterm-send-key (kbd "C-r"))))))

;; eldoc-box + eglot
(require 'eldoc-box)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

;; Line numbers
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)
