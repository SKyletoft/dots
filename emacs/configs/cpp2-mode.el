;;; cpp2-mode.el --- Syntax highlighting for Cpp2 -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst cpp2-highlights
  '(("\\(\\*\\(?:.\\|\n\\)*?\\*/\\|//\\(?:.\\|\n\\)*?$\\)\\|;" . 'font-lock-comment-face)
    ("\\(\\b[A-Z]\\([A-Z]\\|[a-z]\\|[0-9]\\|-\\|_\\)*\\b\\)\\|\\(\\b\\(i8\\|i16\\|i32\\|i64\\|u8\\|u16\\|u32\\|u64\\|f32\\|f64\\|bool\\|char\\|size_t\\|ssize_t\\|void\\)\\b\\)" . 'font-lock-type-face)
    ("[0-9]+" . 'font-lock-number-face)
    ("\\b\\(true\\|false\\)\\b" . 'font-lock-constant-face)
    ("+\\|-\\|*\\|/\\||\\|&&\\||\\|&\\|->\\|::\\|==\\|!\\|!=\\|<\\|>\\|<=\\|>==\\|!\\|?\\|:\\|:=\\|=" . 'font-lock-operator-face)
    ("\\b\\(public\\|fun\\|return\\|yield\\|in\\|out\\|inout\\|move\\|for\\|do\\|while\\|if\\|else\\|switch\\|case\\|inspect\\|operator\\|import\\|export\\|type\\|namespace\\|as\\)\\b" . 'font-lock-keyword-face)
    ("this" . 'font-lock-function-call-face)
    ("std" . 'font-lock-function-name-face)
    ("\\b([A-Z]\\|[a-z]\\|[0-9]\\|-\\|_)+\\b" . 'font-lock-variable-name-face)
    ("#define\\|#include\\|#if\\|#endif\\|#else" . 'font-lock-preprocessor-face)))

(require 'js)
(define-derived-mode cpp2-mode prog-mode "Cpp2"
  "Major mode for editing Cpp2 code."
  (setq-local indent-line-function #'js-indent-line
              font-lock-defaults '(cpp2-highlights)))

(setq auto-mode-alist
      (append (list (cons "\\.cpp2$"  'cpp2-mode)) auto-mode-alist))

(provide 'cpp2-mode)
;;; cpp2-mode.el ends here.
