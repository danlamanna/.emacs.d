(use-package notmuch
  :ensure t
  :config (progn
            ;; notmuch tag:unread
            ;; only show max number emails
            (setq notmuch-search-oldest-first nil)))

(use-package org-notmuch
  :load-path "lisp/"
  :after (org notmuch))

(provide 'dl-mail)
