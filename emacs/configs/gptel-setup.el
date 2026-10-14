;;; gptel-setup.el --- Import gptel and setup tools for it to use  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/agent-shell-maki-make-agent-config ()
    (agent-shell-make-agent-config
     :identifier 'maki
     :mode-line-name "Maki"
     :buffer-name "Maki"
     :shell-prompt "Maki> "
     :shell-prompt-regexp "Maki> "
     :client-maker (lambda (buffer)
                     (agent-shell--make-acp-client
                      :command "maki"
                      :command-params '("acp")
                      :context-buffer buffer))
     :install-instructions "Install maki via home-manager (inputs.maki) or see https://maki.sh/docs/quick-start/."))

(use-package agent-shell
  :config
  (add-to-list 'agent-shell-agent-configs #'my/agent-shell-maki-make-agent-config)
  (setq agent-shell-preferred-agent-config 'opencode
        ;; agent-shell-preferred-agent-config 'maki
        agent-shell-inhibit-system-sleep nil)
  :hook
  (agent-shell-mode . (lambda () (setq-local buffer-undo-list t))))

(provide 'gptel-setup)
;;; gptel-setup.el ends here.
