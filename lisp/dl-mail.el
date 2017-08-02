(use-package helm-notmuch
  :ensure t
  :after (helm notmuch))

(use-package notmuch
  :ensure t
  :bind (:map notmuch-search-mode-map
              ("g" . notmuch-refresh-this-buffer)
              :map notmuch-tree-mode-map
              ("g" . notmuch-refresh-this-buffer))
  :config (progn
            ;; notmuch tag:unread
            ;; only show max number emails
            (setq notmuch-search-oldest-first nil)))

(use-package org-notmuch
  :load-path "lisp/"
  :after (org notmuch))

(use-package sendmail
  :config (progn
            (setq send-mail-function 'sendmail-send-it)))

(provide 'dl-mail)
