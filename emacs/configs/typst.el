(provide 'typst)

(use-package typst-mode)

(evil-define-key 'normal pdf-view-mode-map
  (kbd "C-+") 'pdf-view-enlarge
  (kbd "C--") 'pdf-view-shrink)

(defvar-local typst-is-previewing nil)

(add-hook 'evil-normal-state-entry-hook
          (lambda () (if (and (eq major-mode 'typst-mode)
                              typst-is-previewing)
                         (save-buffer))))


(defun preview-typst ()
  (interactive)
  (if typst-is-previewing
      (progn (setq-local typst-is-previewing t)
             (typst-stop-watch))
    (setq-local typst-is-previewing nil)
    (typst-watch)
    (let ((buf-name (replace-regexp-in-string "typ$"
                                              "pdf"
                                              (buffer-file-name))))
      (split-window-horizontally)
      (evil-window-right 1)
      (find-file buf-name))))
