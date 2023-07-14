(setq artemis-highlights
      '(("\\*\\(?:.\\|\n\\)*?\\*/\\|//\\(?:.\\|\n\\)*?$" . 'font-lock-comment-face)
        ("ℕ\\|ℝ\\|ℤ\\|𝔹\\|∀\\|∃\\|𝕋" . 'font-lock-type-face)
        (":\\|=\\|+\\|-\\|*\\|×\\|/\\|÷\\|λ\\|->\\|→" . 'font-lock-keyword-face)
        ("\\*\\(?:.\\|\n\\)*?\\*/\\|//\\(?:.\\|\n\\)*?$" . 'font-lock-comment-face)
        ))

(define-derived-mode artemis-mode prog-mode "Artemis"
  "major mode for editing artemis code."
  (setq font-lock-defaults '(artemis-highlights)))

(provide 'artemis-mode)
