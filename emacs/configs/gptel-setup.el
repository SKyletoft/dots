;;; gptel-setup.el --- Import gptel and setup tools for it to use  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package aider
  :config
  (setenv "OLLAMA_API_BASE" "http://localhost:11434")
  (setq aider-args '("--model" "ollama_chat/qwen3-coder"
                     "--edit-format" "diff-fenced"
                     "--architect"
                     "--light-mode"))

  (defun my-aider-comint-unbind-spc ()
    ;; Remove from the major mode's keymap
    (define-key aider-comint-mode-map (kbd "SPC") nil)
    ;; Remove from Evil normal state bindings in this mode
    (when (boundp 'evil-normal-state-local-map)
      (define-key evil-normal-state-local-map (kbd "SPC") nil))
    ;; Also clear it via evil-define-key just in case
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal aider-comint-mode-map (kbd "SPC") nil)))
  :hook
  (aider-comint-mode . my-aider-comint-unbind-spc))

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
