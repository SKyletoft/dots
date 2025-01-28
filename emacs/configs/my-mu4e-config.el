;;; mu4e-config.el --- Configure the mu4e email client -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org-mime)

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

(setq mu4e-support-org nil)

; ChatGPT's code to enable images
(setq mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
      mu4e-view-show-images t
      mu4e-view-html-max-width nil
      shr-inhibit-images nil
      mu4e-views-html-filter-external-content nil)

                                        ; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.config/emacs/mu4e/.mbsyncrc -a"
      ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
      mu4e-view-prefer-html t
      mu4e-update-interval 180
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)


;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
          (defun my/mu4e-change-headers ()
            (interactive)
            (setq mu4e-headers-fields
                  `((:human-date . 25) ;; alternatively, use :date
                    (:flags . 6)
                    (:from . 22)
                    (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                    (:size . 7)))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
                                        ;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (visual-line-mode)
            (use-hard-newlines -1)
            (flyspell-mode)))

(require 'smtpmail)

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'mu4e-org)

;; ;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "samuel@kyletoft.se" ;;for samuel@kyletoft.se-gmail
        :enter-func (lambda ())
        :leave-func (lambda ())
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches
                         msg '(:from :to :cc :bcc) "samuel@kyletoft.se")))
        :vars '((user-mail-address . "samuel@kyletoft.se")
                (user-full-name . "Samuel Kyletoft")
                (mu4e-sent-folder . "/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].Sent Mail")
                (mu4e-drafts-folder . "/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].drafts")
                (mu4e-trash-folder . "/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].Bin")
                (mu4e-compose-format-flowed . t)

                (message-send-mail-function . smtpmail-send-it)

                (smtpmail-queue-dir . "~/Maildir/samuel@kyletoft.se-gmail/queue/cur")
                (smtpmail-smtp-user . "samuel@kyletoft.se")
                (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                (smtpmail-default-smtp-server . "smtp.gmail.com")
                (smtpmail-smtp-server . "smtp.gmail.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-debug-info . t)
                (smtpmail-debug-verbose . t)

                (mu4e-maildir-shortcuts . ( ("/samuel@kyletoft.se-gmail/INBOX"            . ?i)
                                            ("/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].Sent Mail" . ?s)
                                            ("/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].Bin"       . ?t)
                                            ("/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].All Mail"  . ?a)
                                            ("/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].Starred"   . ?r)
                                            ("/samuel@kyletoft.se-gmail/[samuel@kyletoft.se].drafts"    . ?d)
                                            ))))))

;; mu4e toggle html images
(defvar killdash9/mu4e~view-html-images nil
  "Whether to show images in html messages.")

(defun killdash9/mu4e-view-toggle-html-images ()
  "Toggle image-display of html message."
  (interactive)
  (setq-local killdash9/mu4e~view-html-images (not killdash9/mu4e~view-html-images))
  (message "Images are %s" (if killdash9/mu4e~view-html-images "on" "off"))
  (mu4e-view-refresh))

(defun mu4e-shr2text (msg)
  "Convert html in MSG to text using the shr engine.
This can be used in `mu4e-html2text-command' in a new enough Emacs.
Based on code by Titus von der Malsburg."
  (lexical-let ((view-images killdash9/mu4e~view-html-images))
               (mu4e~html2text-wrapper
                (lambda ()
                  (let ((shr-inhibit-images (not view-images)))
                    (shr-render-region (point-min) (point-max)))) msg)))

(define-key mu4e-view-mode-map "i" 'killdash9/mu4e-view-toggle-html-images)

(defun mu4e-views-view-raw-html (html msg win)
  (let ((buf (find-file-noselect html)))
    (with-current-buffer buf
      (read-only-mode)
      (select-window win)
      (switch-to-buffer buf t t))))

(defun mu4e-views-view-raw-html-is-view-p (win)
  (let ((winbuf (window-buffer win)))
    (with-current-buffer winbuf
      (eq major-mode 'html-mode))))

;; (add-to-list 'mu4e-views-view-commands
;;              '("rawhtml" .
;;                (:viewfunc mu4e-views-view-raw-html
;;                           :is-view-window-p mu4e-views-view-raw-html-is-view-p)))

(provide 'my-mu4e-config)
;;; my-mu4e-config.el ends here.
