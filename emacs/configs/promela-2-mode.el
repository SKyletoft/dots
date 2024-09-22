;;; promela-2-mode.el --- Syntax highlighting for Promela -*- lexical-binding: t -*-
;;; Commentary:
;;; It's as simple as possible.  I messed around a bit with indents and decided it was too much of a pain
;;; Code:

(defconst promela-2-highlights
  '(("\\*\\(?:.\\|\n\\)*?\\*/\\|//\\(?:.\\|\n\\)*?$" . 'font-lock-comment-face)
    ("\\b\\(proctype\\|int\\|byte\\|bool\\|short\\|unsigned\\|chan\\)\\b" . 'font-lock-type-face)
    ("[0-9]+" . 'font-lock-number-face)
    ("+\\|-\\|*\\|/\\|->\\|→\\|::\\|==\\|!\\|!=\\|<\\|>\\|<=\\|>==\\|!\\|?" . 'font-lock-operator-face)
    ("\\b\\(for\\|in\\|ltl\\|break\\|init\\|active\\|if\\|fi\\|do\\|od\\|assert\\|goto\\|atomic\\|else\\|inline\\|of\\)\\b" . 'font-lock-keyword-face)
    ("#define\\|#include\\|#if\\|#endif\\|#else" . 'font-lock-preprocessor-face)))

(define-derived-mode promela-2-mode prog-mode "Promela"
  "major mode for editing promela code."
  (setq font-lock-defaults '(promela-2-highlights)))

(setq auto-mode-alist
      (append
       (list (cons "\\.promela$"  'promela-2-mode)
             (cons "\\.spin$"     'promela-2-mode)
             (cons "\\.pml$"      'promela-2-mode))
       auto-mode-alist))

(add-hook 'promela-2-mode-hook
          (lambda ()
            (setq-local compile-command (concat "spin " (buffer-file-name)))))

(provide 'promela-2-mode)
;;; promela-2-mode.el ends here.
