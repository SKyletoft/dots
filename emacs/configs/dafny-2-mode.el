;;; dafny-2-mode.el --- A dafny mode that's just syntax hl and lsp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'js)
(require 'lsp)

(defconst dafny-defuns
  '("class" "codatatype" "const" "constructor" "datatype" "function"
    "iterator" "lemma" "method" "newtype" "predicate" "trait" "type"
    "function method" "predicate method" "module"
    "least predicate" "greatest predicate" "least lemma" "greatest lemma"))

(defconst dafny-specifiers
  '("decreases" "ensures" "invariant" "modifies" "provides" "reads" "requires" "witness"))

(defconst dafny-modifiers '("abstract" "ghost" "nameonly" "static" "twostate"))

(defconst dafny-builtins '("extends" "refines" "returns" "yields"))

(defconst dafny-macros '("import" "opened" "export" "reveals" "provides" "include"))

(defconst dafny-keywords
  '("allocated" "as" "assert" "break" "by" "calc" "case" "downto" "else" "exists" "expect" "false"
    "for" "forall" "fresh" "if" "in" "is" "label" "match" "modify" "new" "null" "old"
    "print" "return" "reveal" "then" "this" "to" "true" "unchanged" "var"
    "while" "yield"))

(defconst dafny-dangerous '("assume"))

(defconst dafny-types
  '("array" "array?" "array2" "array2?" "array3" "array3?"
    "bool"
    "bv0" "bv1" "bv2" "bv3" "bv4" "bv5" "bv6" "bv7" "bv8" "bv9"
    "bv10" "bv11" "bv12" "bv13" "bv14" "bv15" "bv16"
    "bv24" "bv32" "bv64" "bv128" "bv256" "bv512" "bv1024" "bv2048"
    "char" "imap" "int" "iset" "map" "multiset" "nat" "object" "object?"
    "real" "seq" "set" "string" "ORDINAL"))

(defconst dafny-defuns-regexp     (regexp-opt dafny-defuns 'symbols))
(defconst dafny-specifiers-regexp (regexp-opt dafny-specifiers 'symbols))
(defconst dafny-modifiers-regexp  (regexp-opt dafny-modifiers 'symbols))
(defconst dafny-builtins-regexp   (regexp-opt dafny-builtins 'symbols))
(defconst dafny-macros-regexp     (regexp-opt dafny-macros 'symbols))
(defconst dafny-keywords-regexp   (regexp-opt dafny-keywords 'symbols))
(defconst dafny-dangerous-regexp  (regexp-opt dafny-dangerous 'symbols))
(defconst dafny-types-regexp      (regexp-opt dafny-types 'symbols))

(defface dafny-dangerous-face
  '((t . ( :box (:line-width -1 :color "#ef2929" :style nil)
            :background "#cc0000"
            :foreground "#eeeeec"
            :weight bold
            :inherit font-lock-warning-face)))
  "Face for unsafe keywords.")

(defconst dafny-font-lock-keywords
  (list
   (cons dafny-defuns-regexp font-lock-builtin-face)
   (cons dafny-modifiers-regexp font-lock-preprocessor-face)
   (cons dafny-specifiers-regexp font-lock-doc-face)
   (cons dafny-builtins-regexp font-lock-builtin-face)
   (cons dafny-macros-regexp font-lock-preprocessor-face)
   ;; (cons "!\\_<in\\_>" font-lock-keyword-face)
   (cons dafny-keywords-regexp font-lock-keyword-face)
   `(,dafny-dangerous-regexp . 'dafny-dangerous-face)
   (cons dafny-types-regexp font-lock-type-face)
   ;; (list "\\(\\_<forall\\_>\\).*?::"
         ;; '(1 (compose-region (match-beginning 1) (match-end 1) ?∀))
   ;; '(1 font-lock-keyword-face append))
   )
  "Font lock specifications for `dafny-2-mode'.")

(defconst dafny-2-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ??  "w" tbl)
    (modify-syntax-entry ?'  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    (modify-syntax-entry ?<  "." tbl)
    (modify-syntax-entry ?>  "." tbl)
    ;; Comments
    (modify-syntax-entry ?\n ">" tbl)
    (modify-syntax-entry ?/  ". 124" tbl)
    (modify-syntax-entry ?*  ". 23bn" tbl)
    tbl)
  "Syntax table for `dafny-2-mode'.")

(define-derived-mode dafny-2-mode prog-mode "dafny"
  "Major mode for editing Dafny code."
  :syntax-table dafny-2-mode-syntax-table
  (setq-local font-lock-defaults '(dafny-font-lock-keywords)
              indent-line-function #'js-indent-line
              comment-start "//"
              comment-end ""))

(setq auto-mode-alist
      (append
       (list (cons "\\.dfy$"  'dafny-2-mode))
       auto-mode-alist))

(add-hook 'dafny-2-mode-hook
          (lambda () (setq-local compile-command (concat "dafny verify " (buffer-file-name)))))

(defun do-nothing ())
(add-to-list 'lsp-language-id-configuration '(dafny-2-mode . "dafny"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("dafny" "server"))
                  :activation-fn (lsp-activate-on "dafny")
                  :major-modes '(dafny-2-mode)
                  :server-id 'dafny
                  :notification-handlers (ht
                                          ("serverStarted" #'do-nothing)
                                          ("dafnyLanguageServerVersionReceived" #'do-nothing)
                                          ("dafny/compilation/status" #'do-nothing)
                                          ("dafny/textDocument/symbolStatus" #'do-nothing))))

(provide 'dafny-2-mode)
;;; dafny-2-mode.el ends here.
