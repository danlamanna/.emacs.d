(use-package org
  :init (progn
          (use-package org-bullets
            :ensure t
            :init (add-hook 'org-mode-hook #'org-bullets-mode))

          (add-hook 'org-mode-hook #'toggle-truncate-lines))
  :commands (org-mode org-agenda org-capture)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (progn
            (setq dl-org-property-icon-map '(("GHISSUE" . "!")
                                             ("GHPR" . "↰")))

            (defun dl-org-icons()
              (s-pad-right 4 " "
                           (s-concat
                            (dl-org-maybe-link-icon)
                            (dl-org-property-icons '("GHISSUE" "GHPR")))))

            (defun dl-org-maybe-link-icon()
              (if (car (org-offer-links-in-entry (current-buffer) (point) 0))
                  "🖇" ""))

            (defun dl-org-property-icons(props)
              (s-join "" (-map (lambda(prop)
                                 (if (org-entry-properties (point) prop)
                                     (cdr (assoc prop dl-org-property-icon-map))
                                   " ")) props)))
            ;; org-babel
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((sh . t)
               (emacs-lisp . nil)
               (python . t)))

            (setq org-ctrl-k-protect-subtree t)
            (setq org-catch-invisible-edits 'error)

            (setq org-replace-disputed-keys t)

            (custom-set-variables
             '(org-enforce-todo-dependencies t)
             '(org-agenda-dim-blocked-tasks 'invisible)
             '(org-agenda-files '("~/org" "~/kworg"))
             '(org-agenda-custom-commands
               '(("w" "Work"
                  ((agenda "")
                   (todo "TODO")
                   (todo "IN REVIEW")
                   (todo "ON HOLD"))
                  ((org-agenda-files '("~/kworg"))))
                 ("b" "Work + Backlog"
                  ((agenda "")
                   (todo "TODO")
                   (todo "IN REVIEW")
                   (todo "ON HOLD")
                   (todo "BACKLOG"))
                  ((org-agenda-files '("~/kworg"))))
                 ("p" "Personal"
                  ((agenda "")
                   (todo "TODO")
                   (todo "ON HOLD")
                   (todo "BACKLOG"))
                  ((org-agenda-files '("~/org"))))
                 ("m" "Mixed"
                  ((agenda "")
                   (todo "TODO")
                   (todo "ON HOLD")
                   (todo "BACKLOG"))
                  ((org-agenda-files '("~/org" "~/kworg"))))))
             '(org-agenda-weekend-days nil) ;; stop highlighting saturday/sunday
             '(org-agenda-skip-deadline-if-done t)
             '(org-agenda-skip-scheduled-if-done t)
             '(org-agenda-start-on-weekday nil) ;; start agenda calendar on today
             '(org-agenda-ndays 5)
             '(org-agenda-prefix-format '((agenda . "f %i %-12:c%?-12t% s")
                                          (timeline . "  % s")
                                          (todo . " %i %(dl-org-icons)")
                                          (tags . " %i %-12:c")
                                          (search . " %i %-12:c")))
             '(org-show-siblings '((default . nil)
                                   (isearch t)
                                   (bookmark-jump t)
                                   (agenda t)))
             '(org-refile-use-outline-path 'file)
             '(org-image-actual-width nil)
             '(org-outline-path-complete-in-steps nil)
             '(org-refile-allow-creating-parent-nodes 'confirm)
             '(org-refile-targets '((org-agenda-files :maxlevel . 8)))
             '(org-todo-keywords '((sequence "TODO(t!)" "IN REVIEW(r!)" "ON HOLD(h@)" "BACKLOG(b!)" "|" "DONE(d!)" "INVALID(i@)")))
             '(org-use-fast-todo-selection t)
             '(org-log-done 'time)
             '(org-capture-templates
               '(("w" "Work TODO" entry
                  (file+headline "~/kworg/notes.org" "Unfiled")
                  "* TODO %?
 %i")
                 ("p" "Personal TODO" entry
                  (file+headline "~/org/todo.org" "Unfiled")
                  "* TODO %?
 %i"))))))


(provide 'dl-org)
