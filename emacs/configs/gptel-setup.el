;;; gptel-setup.el --- Import gptel and setup tools for it to use  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package aider
  :config
  (setenv "OLLAMA_API_BASE" "http://localhost:11434")
  (setq aider-args '("--model ollama_chat/qwen3-coder")))

(use-package gptel
  :ensure t)

(setq gptel-model 'llama3.2:latest
      gptel-backend (gptel-make-ollama "Ollama"
                      :host "localhost:11434"
                      :stream t
                      :models '(llama3.2:latest)))

;; Tools

(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live" buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
                     :type string
                     :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(gptel-make-tool
 :name "save_buffer"
 :function (lambda (buffer-name)
             (let ((buf (get-buffer buffer-name)))
               (if (not buf)
                   (format "Buffer %s does not exist" buffer-name)
                 (with-current-buffer buf
                   (if (not buffer-file-name)
                       (format "Buffer %s has no associated file" buffer-name)
                     (save-buffer)
                     (format "Buffer %s saved to %s" buffer-name buffer-file-name))))))
 :description "Save a buffer to its associated file"
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to save"))
 :category "emacs")

;; (gptel-make-tool
;;  :name "create_file"
;;  :function (lambda (path filename content)
;;              (let ((full-path (expand-file-name filename path)))
;;                (with-temp-buffer
;;                  (insert content)
;;                  (write-file full-path))
;;                (format "Created file %s in %s" filename path)))
;;  :description "Create a new file with the specified content"
;;  :args (list '(:name "path"
;;                      :type string
;;                      :description "The directory where to create the file")
;;              '(:name "filename"
;;                      :type string
;;                      :description "The name of the file to create")
;;              '(:name "content"
;;                      :type string
;;                      :description "The content to write to the file"))
;;  :category "filesystem")

(gptel-make-tool
 :name "create_buffer"
 :function (lambda (buffer-name content)
             (let ((buf (get-buffer-create buffer-name)))
               (with-current-buffer buf
                 (erase-buffer)
                 (insert content))
               (switch-to-buffer buf)
               (format "Created and switched to buffer: %s" buffer-name)))
 :description "Create a new buffer with optional content and switch to it"
 :args (list '(:name "buffer-name"
                     :type string
                     :description "The name of the buffer to create")
             '(:name "content"
                     :type string
                     :description "Initial content to insert in the buffer"))
 :category "emacs")


(gptel-make-tool
 :name "read_file"
 :function (lambda (filepath)
             (when (file-exists-p filepath)
               (with-temp-buffer
                 (insert-file-contents filepath)
                 (buffer-string))))
 :description "Read and return the contents of a file"
 :args (list '(:name "filepath"
                     :type string
                     :description "Path to the file to read"))
 :category "filesystem")

(gptel-make-tool
 :name "list_directory"
 :function (lambda (path)
             (when (file-directory-p path)
               (directory-files path nil "^[^.]+")))
 :description "List the contents of a directory, excluding hidden files"
 :args (list '(:name "path"
                     :type string
                     :description "Path of the directory to list"))
 :category "filesystem")

(gptel-make-tool
 :name "fd_search"
 :function (lambda (query path)
             (shell-command-to-string
              (format "fd %s %s" (shell-quote-argument query) (shell-quote-argument path))))
 :description "Search for files using fd"
 :args (list '(:name "query"
                     :type string
                     :description "The search query (filename pattern)")
             '(:name "path"
                     :type string
                     :description "Directory path to search in"))
 :category "search")

(gptel-make-tool
 :name "ripgrep_search"
 :function (lambda (query path)
             (shell-command-to-string
              (format "rg --no-heading --color never %s %s"
                      (shell-quote-argument query)
                      (shell-quote-argument path))))
 :description "Search for content in files using ripgrep"
 :args (list '(:name "query"
                     :type string
                     :description "The text pattern to search for")
             '(:name "path"
                     :type string
                     :description "Directory path to search in"))
 :category "search")

(gptel-make-tool
 :name "lsp_format_buffer"
 :function (lambda ()
             (when (fboundp 'lsp-format-buffer)
               (lsp-format-buffer)
               "Buffer formatted using LSP"))
 :description "Format the current buffer using LSP"
 :args nil
 :category "lsp")

(gptel-make-tool
 :name "cargo_clippy"
 :function (lambda (&optional path)
             (let ((default-directory (or path default-directory)))
               (shell-command-to-string "cargo clippy")))
 :description "Run cargo clippy for linting a Rust project"
 :args (list '(:name "path"
                     :type string
                     :description "Optional path to the Rust project (default is current directory)"))
 :category "rust")

(defun confirm-before-call (fn prompt)
  "Wrap FN with a yes-or-no confirmation using PROMPT."
  (lambda (&rest args)
    (if (yes-or-no-p prompt)
        (apply fn args)
      "Operation cancelled by user.")))
(gptel-make-tool
 :name "run_command"
 :function (confirm-before-call
            (lambda (command)
              (shell-command-to-string command))
            "Run shell command? ")
 :description "Run a shell command and return its output (with confirmation)"
 :args (list '(:name "command"
                     :type string
                     :description "The shell command to execute"))
 :category "emacs")

(provide 'gptel-setup)
;;; gptel-setup.el ends here.
