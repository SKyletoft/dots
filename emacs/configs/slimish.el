;;; slimish.el --- Shortcuts to quickly and easily copy code to a repl  -*- lexical-binding: t -*-
;;; Commentary:
;;; Depends on vterm and multi-vterm
;;; Code:

(defun vterm-slime ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when (get-buffer "paste-vterm")
      (save-excursion)
      (switch-to-buffer "paste-vterm")
      (if (eq major-mode
              'fundamental-mode)
          (kill-buffer "paste-vterm")))
    (when (not (get-buffer "paste-vterm"))
      (split-window-horizontally)
      (windmove-right)
      (multi-vterm)
      (rename-buffer "paste-vterm"))))

(defun send-to-vterm ()
"Paste the clipboard into the ghci session wrapped in :{ :}."
  (let ((b (current-buffer)))
    (switch-to-buffer "paste-vterm")
    (vterm-yank)
    (vterm-send-return)
    (vterm-send-return)
    (switch-to-buffer b)))

(defun slime-n ()
"Copy the current paragraph and send it to ghci."
  (interactive)
  (save-mark-and-excursion
    (copy-paragraph)
    (send-to-vterm)))

(defun slime-buf ()
"Copy the current selection and send it to ghci."
  (interactive)
  (mark-whole-buffer)
  (kill-ring-save (region-beginning) (region-end))
  (send-to-vterm))

(defun slime-v ()
"Copy the current selection and send it to ghci."
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (send-to-vterm))

(defun ghci ()
"Spawn a ghci terminal in a buffer named ghci.  If currently in haskell-mode, load the current file."
  (interactive)
  (let ((file-name (buffer-file-name))
        (is-haskell (or (eq major-mode 'haskell-mode)
                        (eq major-mode 'haskell-ts-mode))))
    (when (not (get-buffer "ghci"))
      (split-window-horizontally)
      (windmove-right)
      (multi-vterm)
      (rename-buffer "ghci")
      ; $ command -v ghci &> /dev/null && command ghci || nix-shell -p ghc --run "command ghci"
      (vterm-insert "ghci -XGHC2021 -XLambdaCase ")
      (if is-haskell
          (vterm-insert file-name))
      (vterm-send-return)
      (vterm-send "C-l"))))

(defun kill-ghci ()
  (interactive)
  (kill-buffer "ghci"))

(defun python-repl ()
  (interactive)
  (let ((file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
        (is-python (eq major-mode 'python-ts-mode)))
    (vterm-slime)
    (vterm-insert "python3 ")
    (vterm-send-return)
    (if is-python
        (vterm-insert (concat "from "
                              file-name
                              " import *")))
    (vterm-send-return)))

(defun copy-function ()
"Mark and copy the current function as defined by tree sitter."
  (evil-visual-state)
  (call-interactively 'evil-a-paragraph)
  (condition-case nil
      (call-interactively 'evil-textobj-tree-sitter-function--function.outer)
    (error nil))
  (call-interactively 'evil-yank))

(defun copy-paragraph ()
"Mark and copy the current paragraph."
  (backward-paragraph)
  (let ((start (point)))
    (forward-paragraph)
    (let ((end (point)))
      (kill-ring-save start end))))

(defun send-to-ghci ()
"Paste the clipboard into the ghci session wrapped in :{ :}."
  (let ((b (current-buffer)))
    (switch-to-buffer "ghci")
    (vterm-insert ":{\n")
    (vterm-yank)
    (vterm-send-return)
    (vterm-insert ":}")
    (vterm-send-return)
    (switch-to-buffer b)))

(defun hs-slime-n ()
"Copy the current paragraph and send it to ghci."
  (interactive)
  (save-mark-and-excursion
    (copy-paragraph)
    (send-to-ghci)))

(defun hs-slime-v ()
"Copy the current selection and send it to ghci."
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (send-to-ghci))

(defun hs-slime-dwim ()
"Do hs-slime-n or hs-slime-v depending on if there's a current selection."
  (interactive)
  (if (region-active-p)
      (hs-slime-v)
    (hs-slime-n)))

(defun hs-run ()
"Reload the file in ghci and run `main`'."
  (interactive)
  (save-mark-and-excursion
    (let ((b (current-buffer)))
      (switch-to-buffer "ghci")
      (vterm-insert ":r\nmain")
      (vterm-send-return)
      (switch-to-buffer b))))

(provide 'slimish)
;;; slimish.el ends here
