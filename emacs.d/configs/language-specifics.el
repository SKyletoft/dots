(provide 'language-specifics)


(setq-default indent-tabs-mode t)
(setq tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(setq read-process-output-max (* 1024 1024)) ;; LSP perf hack

(setq-default python-indent-offset tab-width)
(setq-default rust-indent-offset tab-width)
(setq-default c-indent-offset tab-width)
(setq-default js-indent-offset tab-width)
(setq-default lisp-indent-offset tab-width)

(use-package eglot)
