(use-package org
  :init (progn
          (use-package org-bullets
            :init (add-hook 'org-mode-hook #'org-bullets-mode))

          (require 'windmove)

          (add-hook 'org-mode-hook #'toggle-truncate-lines)

          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right))
  :commands (org-mode org-agenda org-capture)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config (progn
            ;; org-babel
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((sh . t)
               (emacs-lisp . nil)
               (python . t)))

            (setq org-replace-disputed-keys t)

            (custom-set-variables
             '(org-enforce-todo-dependencies t)
             '(org-agenda-dim-blocked-tasks 'invisible)
             '(org-agenda-files '("~/org" "~/kworg"))
             '(org-agenda-custom-commands
               '(("w" "Work"
                  ((agenda "")
                   (todo "TODO")
                   (todo "ON HOLD")
                   (todo "BACKLOG"))
                  ((org-agenda-files '("~/kworg" "~/kworg/calendars/dan.lamanna.org"))))
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
             '(org-show-siblings '((default . nil)
                                   (isearch t)
                                   (bookmark-jump t)
                                   (agenda t)))
             '(org-refile-use-outline-path 'file)
             '(org-image-actual-width nil)
             '(org-outline-path-complete-in-steps nil)
             '(org-refile-allow-creating-parent-nodes 'confirm)
             '(org-refile-targets '((org-agenda-files :maxlevel . 8)))
             '(org-todo-keywords '((sequence "TODO(t!)" "ON HOLD(h@)" "BACKLOG(b!)" "|" "DONE(d!)" "INVALID(i@)")))
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

(setq org-agenda-prefix-format
      '((agenda . "f %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %(dl-org-property-icons '(\"MU4EMAIL\" \"GHISSUE\" \"GHPR\")) ")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(setq dl-org-property-icon-map '(("GHISSUE" . "!")
                                 ("GHPR" . "↰")
                                 ("MU4EMAIL" . "📪")))



(defun dl-org-property-icons(props)
  (s-pad-right 3 " "
               (s-join "" (-map (lambda(prop)
                                  (if (org-entry-properties (point) prop)
                                      (cdr (assoc prop dl-org-property-icon-map))
                                    " ")) props))))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
