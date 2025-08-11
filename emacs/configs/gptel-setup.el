;;; gptel-setup.el --- Import gptel and setup tools for it to use  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package aider
  :config
  (setenv "OLLAMA_API_BASE" "http://localhost:11434")
  (setq aider-args '("--model ollama_chat/qwen3-coder")))

(use-package gptel
  :ensure t)

(setq gptel-model 'qwen3-coder
      gptel-backend (gptel-make-ollama "Ollama"
                      :host "localhost:11434"
                      :stream t
                      :models '(qwen3-coder gpt-oss)))

;; Tools
(use-package gptel-tool-library
  :load-path "~/dots/emacs/third-party/gptel-tool-library/"
  :config
  (dolist (module '(
                    ; "bbdb"
                    "buffer"
                    "elisp"
                    "emacs"
                    ;; "gnus"
                    "os"
                    ))
    (gptel-tool-library-load-module module)))

(provide 'gptel-setup)
;;; gptel-setup.el ends here.
