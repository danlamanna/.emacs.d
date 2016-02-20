(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :demand t
  :config (progn
            (require 'org-mu4e)
            (require 'mu4e-configs)
            ;; Don't include self in replies CC
            (setq mu4e-compose-dont-reply-to-self t)
            (setq mu4e-sent-messages-behavior 'delete)

            (setq mu4e-maildir-shortcuts
                  '(("/INBOX" . ?i)))

            (setq
             user-mail-address "dan.lamanna@kitware.com"
             user-full-name "Dan LaManna"
             mu4e-compose-signature "- Dan")

            (require 'smtpmail)

            (setq message-send-mail-function 'smtpmail-send-it
                  smtpmail-stream-type 'starttls
                  smtpmail-default-smtp-server "smtp.gmail.com"
                  smtpmail-smtp-server "smtp.gmail.com"
                  smtpmail-smtp-service 587)

            ;; don't keep message buffers around
            (setq message-kill-buffer-on-exit t)



            (setq mu4e-html2text-command "html2text -utf8 -width 72")))

(provide 'dl-mail)

; helm mu, helm mu contacts
