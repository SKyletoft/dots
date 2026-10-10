;;; ispc-ts-mode.el --- Tree-sitter support for ISPC -*- lexical-binding: t; -*-

;; Author: None (Vibe coded; no copyright)
;; Keywords: languages tree-sitter ispc

;; This file is not a part of GNU Emacs.

;;; Commentary:
;;
;; This package provides `ispc-ts-mode' for ISPC
;; (https://ispc.github.io), based on the builtin `c-ts-mode'.
;; ISPC is C-like and its tree-sitter grammar is an extension of
;; tree-sitter-c, so font-lock reuses the builtin C helpers for
;; declarations (`c-ts-mode--fontify-declarator' and friends).
;;

;;; Code:

(require 'treesit)
(require 'c-ts-mode) ; For `c-ts-mode--syntax-table' and the declarator helpers.
(eval-when-compile (require 'rx))

(if (>= emacs-major-version 31)
    (treesit-declare-unavailable-functions)
  (progn
    (declare-function treesit-parser-create "treesit.c")
    (declare-function treesit-node-child-by-field-name "treesit.c")
    (declare-function treesit-node-child "treesit.c")
    (declare-function treesit-node-parent "treesit.c")
    (declare-function treesit-node-type "treesit.c")
    (declare-function treesit-node-text "treesit.c")))

(add-to-list
 'treesit-language-source-alist
 '(ispc "https://github.com/tree-sitter-grammars/tree-sitter-ispc"
        :rev "ba1bb38ac8ddfa6aa7571cbfe9b4d029f7f77447")
 t)

(defgroup ispc-ts nil
  "Major mode for editing ISPC, powered by tree-sitter."
  :group 'languages
  :prefix "ispc-ts-mode-")

(defcustom ispc-ts-mode-indent-offset 8
  "Number of spaces for each indentation step in `ispc-ts-mode'."
  :version "30.1"
  :type 'integer
  :safe 'integerp
  :group 'ispc-ts)

;;; Keywords, types and builtins
;;
;; Adapted from Munksgaard/ispc-mode and the upstream
;; queries/highlights.scm of tree-sitter-ispc.  Only strings that are
;; real anonymous tokens in the grammar are listed: `sync' is a
;; single-token rule (matched via its `sync_expression' node
;; instead), the C primitives (`bool', `int', ...) live inside the
;; `primitive_type' token, and `size_t' and friends parse as
;; `type_identifier' nodes.

(defvar ispc-ts-mode--keywords
  '("break" "case" "cdo" "cfor" "cif" "continue" "cwhile"
    "default" "delete" "do" "else" "enum" "extern" "for"
    "foreach" "foreach_active" "foreach_tiled" "foreach_unique"
    "goto" "if" "in" "inline" "launch" "new" "noinline"
    "operator" "return" "sizeof" "soa" "static" "struct"
    "switch" "task" "typedef" "unmasked" "while"
    "template" "typename"
    "__declspec" "__vectorcall" "__regcall")
  "ISPC keywords for tree-sitter font-locking.")

(defvar ispc-ts-mode--type-keywords
  '("const" "export" "float16"
    "int8" "int16" "int32" "int64"
    "uint8" "uint16" "uint32" "uint64"
    "long" "short" "signed" "unsigned"
    "uniform" "varying" "volatile" "ptrdiff_t")
  "ISPC type keywords for tree-sitter font-locking.")

(defvar ispc-ts-mode--builtin-variables
  '("programCount" "programIndex"
    "taskCount" "taskCount0" "taskCount1" "taskCount2" "taskCount3"
    "taskIndex" "taskIndex0" "taskIndex1" "taskIndex2"
    "threadCount" "threadIndex")
  "ISPC builtin variables (SPMD program/task identifiers).")

(defvar ispc-ts-mode--builtin-constants
  '("NULL"
    "DBL_MAX" "DBL_MIN" "F16_MAX" "F16_MIN" "FLT_MAX" "FLT_MIN"
    "INT16_MAX" "INT16_MIN" "INT32_MAX" "INT32_MIN"
    "INT64_MAX" "INT64_MIN" "INT8_MAX" "INT8_MIN" "ISPC"
    "ISPC_ATTRIBUTE_SUPPORTED" "ISPC_LLVM_INTRINSICS_ENABLED"
    "ISPC_MAJOR_VERSION" "ISPC_MINOR_VERSION" "ISPC_POINTER_SIZE"
    "ISPC_TARGET_AVX" "ISPC_TARGET_AVX10_2DMR" "ISPC_TARGET_AVX2"
    "ISPC_TARGET_AVX512GNR" "ISPC_TARGET_AVX512KNL"
    "ISPC_TARGET_AVX512SKX" "ISPC_TARGET_AVX512SPR"
    "ISPC_TARGET_HAS_FP16_SUPPORT" "ISPC_TARGET_HAS_FP64_SUPPORT"
    "ISPC_TARGET_NEON" "ISPC_TARGET_SSE2" "ISPC_TARGET_SSE4"
    "ISPC_TARGET_XE2HPG" "ISPC_TARGET_XE2LPG" "ISPC_TARGET_XEHPC"
    "ISPC_TARGET_XEHPG" "ISPC_TARGET_XELP" "ISPC_TARGET_XELPG"
    "ISPC_UINT_IS_DEFINED" "LLVM_VERSION_MAJOR" "LLVM_VERSION_MINOR"
    "PI" "TARGET_ELEMENT_WIDTH" "TARGET_WIDTH"
    "UINT16_MAX" "UINT32_MAX" "UINT64_MAX" "UINT8_MAX")
  "ISPC preprocessor constants for font-locking.")

(defvar ispc-ts-mode--builtin-functions
  '("abs" "acos" "all" "alloca"
    "amx_dpbf16ps" "amx_dpbssd" "amx_dpbsud" "amx_dpbusd" "amx_dpbuud"
    "amx_dpfp16ps" "amx_tile_load" "amx_tile_loadconfig"
    "amx_tile_load_t1" "amx_tile_release" "amx_tile_store"
    "amx_tile_storeconfig" "amx_tile_zero" "and" "any"
    "aos_to_soa2" "aos_to_soa3" "aos_to_soa4" "asin" "assert" "assume"
    "atan" "atan2"
    "atomic_add_global" "atomic_add_local"
    "atomic_and_global" "atomic_and_local"
    "atomic_compare_exchange_global" "atomic_compare_exchange_local"
    "atomic_max_global" "atomic_max_local"
    "atomic_min_global" "atomic_min_local"
    "atomic_or_global" "atomic_or_local"
    "atomic_subtract_global" "atomic_subtract_local"
    "atomic_swap_global" "atomic_swap_local"
    "atomic_xor_global" "atomic_xor_local"
    "avg_down" "avg_up" "broadcast" "cbrt" "ceil" "clamp" "clock"
    "cos" "cosh" "count_leading_zeros" "count_trailing_zeros"
    "dot2add_i16i16packed" "dot2add_i16i16packed_sat"
    "dot2add_u16i16packed" "dot2add_u16i16packed_sat"
    "dot2add_u16u16packed" "dot2add_u16u16packed_sat"
    "dot4add_i8i8packed" "dot4add_i8i8packed_sat"
    "dot4add_u8i8packed" "dot4add_u8i8packed_sat"
    "dot4add_u8u8packed" "dot4add_u8u8packed_sat"
    "doublebits" "erf" "erfc"
    "exclusive_scan_add" "exclusive_scan_and" "exclusive_scan_or"
    "exp" "expm1" "extract" "fastmath"
    "float16bits" "floatbits" "float_to_half" "float_to_half_fast"
    "float_to_srgb8" "floor" "fmod" "frandom" "frexp"
    "half_to_float" "half_to_float_fast" "insert" "intbits"
    "invoke_sycl" "isfinite" "isinf" "isnan"
    "ISPCAlloc" "ISPCInstrument" "ISPCLaunch" "ISPCSync"
    "lanemask" "ldexp" "log" "log1p" "max"
    "memcpy" "memcpy64" "memmove" "memmove64" "memory_barrier"
    "memset" "memset64" "min" "none" "num_cores" "or"
    "packed_load_active" "packed_store_active" "packed_store_active2"
    "packmask" "popcnt" "pow"
    "prefetch_l1" "prefetch_l2" "prefetch_l3" "prefetch_nt"
    "prefetchw_l1" "prefetchw_l2" "prefetchw_l3"
    "print" "random" "rcp" "rcp_fast" "rdrand"
    "reduce_add" "reduce_equal" "reduce_max" "reduce_min"
    "rotate" "round" "rsqrt" "rsqrt_fast"
    "saturating_add" "saturating_div" "saturating_mul" "saturating_sub"
    "seed_rng" "select" "shift" "shuffle" "signbits" "sign_extend"
    "sin" "sincos" "sinh"
    "soa_to_aos2" "soa_to_aos3" "soa_to_aos4" "sqrt" "srgb8_to_float"
    "streaming_load" "streaming_load_uniform" "streaming_store"
    "tan" "tanh" "trunc")
  "ISPC standard library builtins for font-locking.")

(defvar ispc-ts-mode--builtin-keywords
  `((,(regexp-opt ispc-ts-mode--builtin-variables 'symbols)
     0 font-lock-builtin-face t)
    (,(regexp-opt ispc-ts-mode--builtin-constants 'symbols)
     0 font-lock-constant-face t)
    (,(concat "\\_<" (regexp-opt ispc-ts-mode--builtin-functions) "\\_>[ \t]*(")
     0 font-lock-builtin-face t))
  "Regex keywords for ISPC builtins, layered over tree-sitter font-lock.
Builtin variables and constants match anywhere; standard library
functions only in call position.  Plain regexps on purpose:
equivalent `:match' patterns inside tree-sitter queries crash the
query compiler (see the Commentary).")

;;; Indent
;;
;; Small explicit rule set: braces and labels align with the
;; surrounding line, block contents and statement bodies indent one
;; step, everything else follows its parent.  Multi-line argument
;; lists simply indent rather than aligning to the open paren.

(defvar ispc-ts-mode--indent-rules
  `((ispc
     ((parent-is "translation_unit") column-0 0)
     ;; The indent engine hands us the largest node starting at the
     ;; line, so a lone "{" arrives as its block node, never as "{".
     ((node-is ,(rx (or "compound_statement" "field_declaration_list"
                        "enumerator_list" "initializer_list")))
      parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ,(rx (or "else" "case"))) parent-bol 0)
     ((node-is "labeled_statement") parent-bol 0)
     ((node-is "preproc") column-0 0)
     ((node-is "#endif") column-0 0)
     ((node-is "preproc_arg") no-indent)
     ((match "while" "do_statement") parent 0)
     ((parent-is ,(rx (or "compound_statement"
                          "field_declaration_list"
                          "enumerator_list"
                          "initializer_list")))
      parent-bol ispc-ts-mode-indent-offset)
     ((parent-is ,(rx (or "if_statement" "for_statement"
                          "foreach_statement" "foreach_instance_statement"
                          "while_statement" "do_statement"
                          "unmasked_statement")))
      parent-bol ispc-ts-mode-indent-offset)
     ((parent-is "preproc") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     (catch-all parent-bol ispc-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `ispc-ts-mode'.")

;;; Font-lock

(defvar ispc-ts-mode--feature-list
  '((comment definition)
    (builtin keyword preprocessor string type)
    (assignment constant escape-sequence label literal)
    (bracket delimiter error function operator property variable))
  "`treesit-font-lock-feature-list' for `ispc-ts-mode'.")

(defun ispc-ts-mode--font-lock-settings ()
  "Tree-sitter font-lock settings for `ispc-ts-mode'."
  (treesit-font-lock-rules
   :default-language 'ispc
   :feature 'comment
   `(((comment) @font-lock-doc-face
      (:match ,(rx bos "/**") @font-lock-doc-face))
     (comment) @font-lock-comment-face)

   :feature 'preprocessor
   `((preproc_directive) @font-lock-preprocessor-face
     (preproc_def name: (identifier) @font-lock-variable-name-face)
     (preproc_ifdef name: (identifier) @font-lock-variable-name-face)
     (preproc_function_def name: (identifier) @font-lock-function-name-face)
     [,@c-ts-mode--preproc-keywords] @font-lock-preprocessor-face)

   :feature 'builtin
   '(((llvm_identifier) @font-lock-builtin-face))

   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face)

   :feature 'keyword
   `([,@ispc-ts-mode--keywords] @font-lock-keyword-face
     (sync_expression) @font-lock-keyword-face)

   :feature 'operator
   `([,@c-ts-mode--operators "..."] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :feature 'string
   `((string_literal) @font-lock-string-face
     (system_lib_string) @font-lock-string-face)

   :feature 'literal
   `((number_literal) @font-lock-number-face
     (char_literal) @font-lock-constant-face)

   :feature 'type
   `((primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     (sized_type_specifier) @font-lock-type-face
     (short_vector) @font-lock-type-face
     [,@ispc-ts-mode--type-keywords] @font-lock-type-face)

   :feature 'definition
   `((declaration declarator: (_) @c-ts-mode--fontify-declarator)
     (field_declaration declarator: (_) @c-ts-mode--fontify-declarator)
     (function_definition declarator: (_) @c-ts-mode--fontify-declarator)
     ((function_declarator) @c-ts-mode--fontify-declarator)
     (parameter_declaration declarator: (_) @c-ts-mode--fontify-declarator)
     (enumerator name: (identifier) @font-lock-property-name-face))

   :feature 'assignment
   '((assignment_expression left: (identifier) @font-lock-variable-use-face)
     (assignment_expression
      left: (field_expression field: (_) @font-lock-property-use-face))
     (init_declarator declarator: (_) @c-ts-mode--fontify-declarator))

   :feature 'function
   '((call_expression
      function:
      [(identifier) @font-lock-function-call-face
       (field_expression field: (field_identifier) @font-lock-function-call-face)]))

   :feature 'variable
   '((identifier) @c-ts-mode--fontify-variable)

   :feature 'label
   '((labeled_statement label: (statement_identifier) @font-lock-constant-face))

   :feature 'error
   '((ERROR) @c-ts-mode--fontify-error)

   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'property
   '((field_identifier) @font-lock-property-use-face)

   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)))

;;; Defun navigation and imenu

(defun ispc-ts-mode--inside-template-p (node)
  "Return non-nil if NODE is nested inside a `template_declaration'."
  (let ((parent (treesit-node-parent node))
        (found nil))
    (while (and parent (not found))
      (when (equal (treesit-node-type parent) "template_declaration")
        (setq found t))
      (setq parent (treesit-node-parent parent)))
    found))

(defun ispc-ts-mode--defun-name (node)
  "Return the name of the defun NODE in `ispc-ts-mode'.
Like `c-ts-mode--defun-name', plus `template_declaration' and
`template_instantiation'.  Return nil if NODE has no name."
  (pcase (treesit-node-type node)
    ("template_declaration"
     ;; Named children: `template_parameter_list', then the inner
     ;; declaration or definition (or another template).
     (when-let* ((inner (treesit-node-child node 1 t)))
       (ispc-ts-mode--defun-name inner)))
    ("template_instantiation"
     (treesit-node-text
      (c-ts-mode--declarator-identifier
       (treesit-node-child-by-field-name node "declarator")
       t)
      t))
    (_ (c-ts-mode--defun-name node))))

(defun ispc-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node in `ispc-ts-mode'.
Like `c-ts-mode--defun-valid-p', except a function nested in a
`template_declaration' is not a defun of its own (the template is)."
  (and (c-ts-mode--defun-valid-p node)
       (not (and (member (treesit-node-type node)
                         '("function_definition" "declaration"))
                 (ispc-ts-mode--inside-template-p node)))))

;;; The mode

(defvar-keymap ispc-ts-mode-map
  :doc "Keymap for ISPC with tree-sitter."
  :parent prog-mode-map)

;;;###autoload
(define-derived-mode ispc-ts-mode prog-mode "ISPC"
  "Major mode for editing ISPC, powered by tree-sitter.

\\{ispc-ts-mode-map}"
  :group 'ispc-ts
  :syntax-table c-ts-mode--syntax-table

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))
  (setq-local electric-indent-chars (append "{}():;,#" electric-indent-chars))

  (if (not (treesit-ready-p 'ispc))
      (message "ISPC tree-sitter grammar not available; using fallback setup")
    (treesit-parser-create 'ispc)

    (setq-local treesit-defun-type-regexp
                (regexp-opt '("function_definition"
                              "struct_specifier"
                              "enum_specifier"
                              "union_specifier"
                              "template_declaration"
                              "template_instantiation"
                              "preproc_def"
                              "preproc_function_def")))
    (setq-local treesit-defun-skipper #'c-ts-mode--defun-skipper)
    (setq-local treesit-defun-name-function #'ispc-ts-mode--defun-name)
    (setq-local treesit-defun-tactic 'top-level)

    (setq-local treesit-simple-indent-rules ispc-ts-mode--indent-rules)
    (setq-local treesit-font-lock-settings (ispc-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list ispc-ts-mode--feature-list)

    (setq-local treesit-simple-imenu-settings
                `(("Enum" "\\`enum_specifier\\'"
                   ispc-ts-mode--defun-valid-p nil)
                  ("Struct" "\\`struct_specifier\\'"
                   ispc-ts-mode--defun-valid-p nil)
                  ("Union" "\\`union_specifier\\'"
                   ispc-ts-mode--defun-valid-p nil)
                  ("Template" "\\`template_declaration\\'"
                   ispc-ts-mode--defun-valid-p nil)
                  ("Variable" ,(rx bos "declaration" eos)
                   ispc-ts-mode--defun-valid-p nil)
                  ("Function" "\\`function_definition\\'"
                   ispc-ts-mode--defun-valid-p nil)))

    (treesit-major-mode-setup)
    (font-lock-add-keywords nil ispc-ts-mode--builtin-keywords t)))

(add-to-list 'auto-mode-alist '("\\.ispc\\'" . ispc-ts-mode))
(add-to-list 'auto-mode-alist '("\\.isph\\'" . ispc-ts-mode))

(provide 'ispc-ts-mode)
;;; ispc-ts-mode.el ends here
