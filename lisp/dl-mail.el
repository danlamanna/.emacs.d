(use-package helm-notmuch
  :ensure t
  :after (helm notmuch))

(use-package hydra
  :ensure t
  :after (helm notmuch)
  :config (progn
            (defhydra hydra-mail (global-map "C-c m")
              "mail"
              ("s" helm-notmuch "search" :exit t)
              ("r" (notmuch-tree "tag:unread -tag:ignore") "read" :exit t)
              ("d" (notmuch-search "tag:draft") "drafts" :exit t)
              ("c" compose-mail "compose" :exit t)
              ("o" make-frame-command "new-frame" :color red))))

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
