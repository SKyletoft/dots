(provide 'irc-conf)

;; Set our nickname & real-name as constant variables
(setq
 erc-nick "u3836"     ; Our IRC nick
 erc-user-full-name "Samuel") ; Our /whois name

;; Define a function to connect to a server
(defun irk ()
  (interactive)
  (erc :server "irc.dtek.se"
       :port   "6667"))

;; Or assign it to a keybinding
;; This example is also using erc's TLS capabilities:
;; (global-set-key "\C-cen"
  ;; (lambda ()
  ;; (interactive)
  ;; (erc-tls :server "server2.example.com"
           ;; :port   "6697")))
