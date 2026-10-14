;;; gptel-setup.el --- Import gptel and setup tools for it to use  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package agent-shell
  :config
  (setq agent-shell-preferred-agent-config 'opencode)
  :hook
  (agent-shell-mode . (lambda () (setq-local buffer-undo-list t))))

(provide 'gptel-setup)
;;; gptel-setup.el ends here.
