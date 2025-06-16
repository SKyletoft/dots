;; -*- lexical-binding: t -*-

(setq number-highlights
      '(("1" . 'org-level-1)
        ("2" . 'org-level-2)
        ("3" . 'org-level-3)
        ("4" . 'org-level-4)
        ("5" . 'org-level-5)
        ("6" . 'org-level-6)
        ("7" . 'org-level-7)
        ("8" . 'org-level-8) ;
        ("9" . 'font-lock-preprocessor-face)
        ("0" . 'font-lock-doc-face)
        ))

(define-derived-mode number-hl-mode prog-mode "Numbers"
  "Major mode for highlighting numbers"
  (setq font-lock-defaults '(number-highlights)))

(provide 'number-hl-mode)
