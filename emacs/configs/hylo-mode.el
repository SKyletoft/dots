;;; hylo-mode.el --- Syntax highlighting for Hylo -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst hylo-highlights
  '(("\\*\\(?:.\\|\n\\)*?\\*/\\|//\\(?:.\\|\n\\)*?$" . 'font-lock-comment-face)
    ("\\b\\(proctype\\|int\\|byte\\|bool\\|short\\|unsigned\\|chan\\|mtype\\)\\b" . 'font-lock-type-face)
    ("[0-9]+" . 'font-lock-number-face)
    ("\\b\\(true\\|false\\)\\b" . 'font-lock-constant-face)
    ("+\\|-\\|*\\|/\\|->\\|→\\|::\\|==\\|!\\|!=\\|<\\|>\\|<=\\|>==\\|!\\|?\\|??" . 'font-lock-operator-face)
    ("\\b\\(public\\|fun\\|return\\|yield\\)\\b" . 'font-lock-keyword-face)
    ("\\b\\(printf\\|eval\\)\\b" . 'font-lock-function-call-face)
    ("#define\\|#include\\|#if\\|#endif\\|#else" . 'font-lock-preprocessor-face)))

(define-derived-mode hylo-mode c-mode "Hylo"
  "Major mode for editing Hylo code."
  (setq font-lock-defaults '(hylo-highlights)))

(setq auto-mode-alist
      (append
       (list (cons "\\.hylo$"  'hylo-mode))
       auto-mode-alist))

(add-hook 'hylo-mode-hook
          (lambda ()
            (set-indents 8 8 t)
            (setq-local compile-command (concat "hc "
                                                (buffer-file-name)
                                                " -o "
                                                (file-name-base (buffer-file-name))))))

(provide 'hylo-mode)
;;; hylo-mode.el ends here.
