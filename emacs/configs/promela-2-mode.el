(defvar promela-indent-offset tab-width
  "Indentation offset for `promela-2-mode'.")

(defconst promela-2-highlights
  '(("/\\*\\(.\\|\n\\)*?\\*/\\|;" . 'font-lock-comment-face)
    ("\\b\\(int\\|byte\\|bool\\|short\\|unsigned\\)\\b" . 'font-lock-type-face)
    ("[0-9]+" . 'font-lock-number-face)
    ("+\\|-\\|*\\|/\\|->\\|→\\|::\\|==\\|!\\|!=\\|<\\|>\\|<=\\|>==" . 'font-lock-operator-face)
    ("\\b\\(active\\|if\\|fi\\|do\\|od\\|assert\\|goto\\|atomic\\|else\\|inline\\)\\b" . 'font-lock-keyword-face)
    ("#define\\|#include\\|#if\\|#endif\\|#else" . 'font-lock-preprocessor-face)))

(defvar promela-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((stmt ("if" exp "then" stmt "else" stmt)
            ("while" exp "do" stmt)
            ("{" stmts "}")
            (stmt ";" stmt))
      (stmts (stmt))
      (exp ("true")
           ("false")
           ("!" exp)
           (exp "&&" exp)
           (exp "||" exp)
           ("(" exp ")"))))))

(defun promela-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) promela-indent-offset)
    (`(:before . ";") (smie-rule-separator kind))
    (`(:after . "{") (smie-rule-parent))
    (`(:before . "}") (smie-rule-parent))))

(define-derived-mode promela-2-mode prog-mode "Promela"
  "major mode for editing promela code."
  (setq font-lock-defaults '(promela-2-highlights))
  (smie-setup promela-smie-grammar #'promela-smie-rules
              :forward-token #'mylang-forward-token
              :backward-token #'mylang-backward-token))

(setq auto-mode-alist
      (append
       (list (cons "\\.promela$"  'promela-2-mode)
             (cons "\\.spin$"     'promela-2-mode)
             (cons "\\.pml$"      'promela-2-mode))
       auto-mode-alist))
(provide 'promela-2-mode)
