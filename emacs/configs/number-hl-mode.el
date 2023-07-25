(setq number-highlights
      '(("1" . 'font-lock-comment-face)
        ("2" . 'font-lock-type-face)
        ("3" . 'font-lock-keyword-face)
        ("4" . 'font-lock-function-name-face)
        ("5" . 'font-lock-string-name-face)
        ("6" . 'font-lock-negation-char-face)
        ("7" . 'font-lock-warning-face)
        ("8" . 'font-lock-constant-face) ;
        ("9" . 'font-lock-preprocessor-face)
        ("0" . 'font-lock-doc-face)
        ))

(define-derived-mode number-hl-mode prog-mode "Numbers"
  "Major mode for highlighting numbers"
  (setq font-lock-defaults '(number-highlights)))

(provide 'number-hl-mode)
