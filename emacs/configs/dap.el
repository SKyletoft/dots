(provide 'dap)

(use-package dap-mode)

(dap-register-debug-provider
  "hda"
  (lambda (conf)
    (plist-put conf :dap-server-path (list
                                      "haskell-debug-adapter"
                                      "--hackage-version=0.0.31.0"))
    conf))

(require 'dap-gdb-lldb)

(dap-register-debug-template "haskell-debug-adapter"
  (list :type "hda"
    :request "launch"
    :name "haskell-debug-adapter"
    :internalConsoleOptions "openOnSessionStart"
    ;; :workspace (lsp-find-session-folder (lsp-session) (buffer-file-name))
    ;; :workspace "C:/work/haskell/sample"
    ;; :startup "C:/work/haskell/sample/app/Main.hs"
    :startupFunc ""
    :startupArgs ""
    :stopOnEntry t
    :mainArgs ""
    ;; :ghciPrompt "H>>= "
    ;; :ghciInitialPrompt "Prelude>"
    ;; :ghciCmd "stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show"
    :ghciEnv (list :dummy "")
    ;; :logFile "C:/work/haskell/sample/hdx4emacs.log"
    :logLevel "WARNING"
    :forceInspect nil))

(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                           :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))
