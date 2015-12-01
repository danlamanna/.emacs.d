(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; package.el
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)

;; must-have libraries/utilities
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defmacro use-package-ensure(package &rest body)
  (declare (indent 1))
  `(use-package ,package
     :ensure t
     ,@body))

(use-package-ensure dash
  :demand t)

;; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro")))))

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(auto-save-interval 60)
 '(bookmark-save-flag t)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(confirm-nonexistent-file-or-buffer nil)
 '(custom-safe-themes
   (quote
    ("30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "c3232d379e847938857ca0408b8ccb9d0aca348ace6f36a78f0f7b4c5df0115c" "f1af57ed9c239a5db90a312de03741e703f712355417662c18e3f66787f94cbe" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" default)))
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(electric-pair-pairs (quote ((34 . 34) (123 . 125))))
 '(enable-recursive-minibuffers t)
 '(guru-warn-only t t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(interprogram-paste-function (quote x-cut-buffer-or-selection-value) t)
 '(js2-mode-show-parse-errors nil)
 '(js2-strict-missing-semi-warning nil)
 '(large-file-warning-threshold 100000000)
 '(magit-auto-revert-mode nil)
 '(magit-diff-arguments (quote ("--ignore-all-space")))
 '(magit-diff-options (quote ("--ignore-all-space")))
 '(magit-use-overlays nil)
 '(mouse-autoselect-window t)
 '(next-line-add-newlines t)
 '(org-agenda-custom-commands
   (quote
    (("w" "Work"
      ((agenda "")
       (todo "TODO")
       (todo "ON HOLD"))
      ((org-agenda-files
        (quote
         ("~/kworg/notes.org")))))
     ("p" "Personal"
      ((agenda "")
       (todo "TODO")
       (todo "ON HOLD"))
      ((org-agenda-files
        (quote
         ("~/org/notes.org" "~/org/csi660.org")))))
     ("m" "Mixed"
      ((agenda "")
       (todo "TODO")
       (todo "ON HOLD"))
      ((org-agenda-files
        (quote
         ("~/org/notes.org" "~/kworg/notes.org"))))))))
 '(org-agenda-files (quote ("~/kworg/notes.org")))
 '(org-agenda-span (quote week))
 '(org-agenda-weekend-days nil)
 '(org-capture-templates
   (quote
    (("w" "Work TODO" entry
      (file+headline "~/kworg/notes.org" "Unfiled")
      "* TODO %?
 %i")
     ("p" "Personal TODO" entry
      (file+headline "~/org/notes.org" "Unfiled")
      "* TODO %?
 %i"))))
 '(org-completion-use-ido t)
 '(org-default-notes-file "/home/dan/org/notes.org")
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 8))))
 '(org-refile-use-outline-path (quote file))
 '(org-show-siblings
   (quote
    ((default)
     (isearch t)
     (bookmark-jump t)
     (agenda t))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t!)" "ON HOLD(h@)" "|" "DONE(d!)"))))
 '(org-use-fast-todo-selection t)
 '(reb-re-syntax (quote string))
 '(require-final-newline t)
 '(save-interprogram-paste-before-kill t)
 '(show-paren-style (quote mixed))
 '(tramp-default-method "ssh")
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(vc-follow-symlinks t)
 '(virtualenv-root "~/.virtualenvs/")
 '(wdired-allow-to-change-permissions t)
 '(x-select-enable-clipboard t)
 '(yank-pop-change-selection t))

;; disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ace-jump-mode
(use-package-ensure ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode-pop-mark)
  :commands (ace-jump-word-mode
             ace-jump-char-mode
             ace-jump-line-mode))

;; ag
(use-package-ensure ag
  :init (custom-set-variables
         '(ag-highlight-search t))
  :config (progn
            ;; make q actually kill the buffer, not just bury it
            (define-key ag-mode-map (kbd "q") '(lambda () (interactive)
                                                 (let (kill-buffer-query-functions) (kill-buffer))))))

;; bookmark
(use-package bookmark
  :demand t
  :config (custom-set-variables
           '(bookmark-save-flag t)))

;; company
(use-package-ensure company)

;; company-tern
(use-package-ensure company-tern
  :config (progn
            (add-to-list 'company-backends 'company-tern)))

;; css-mode
(use-package css-mode
  :config (progn
            (add-hook 'css-mode-hook 'rainbow-mode)))

;; pending-delete-mode/delete-select-mode
(use-package delsel
  :config (pending-delete-mode t))

;; dired
(use-package dired
  :demand t
  :config (progn
            (custom-set-variables
             '(dired-dwim-target t)
             '(dired-recursive-copies 'always)
             '(dired-recursive-deletes 'always)
             '(wdired-allow-to-change-permissions t))

            (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
            (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

            (put 'dired-do-copy 'ido 'find-file)
            (put 'dired-do-rename 'ido 'find-file)

            ;; @todo how do these get ensured?
            ;;(require 'dired-async)
            ;;(require 'dired-x)

            (defun dired-back-to-top ()
              (interactive)
              (beginning-of-buffer)
              (dired-next-line 4))

            (defun dired-jump-to-bottom ()
              (interactive)
              (end-of-buffer)
              (dired-next-line -1))))

;; dl-aesthetics
(use-package dl-aesthetics
  :load-path "lisp/"
  :demand t
  :bind ("<f11>" . toggle-fullscreen))

;; dl-backups
(use-package dl-backups
  :load-path "lisp/"
  :demand t
  :init (progn
          (setq
           auto-save-interval 30
           emacs-autosave-dir (concat emacs-tmp-dir "/autosaves/")
           auto-save-list-file-prefix emacs-autosave-dir
           auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t))
           vc-make-backup-files t ;; backup version controlled files, too
           backup-by-copying t ;; no symlinks
           delete-old-versions t ;; no confirm
           kept-new-versions 20
           kept-old-versions 20
           version-control t ;; number backups
           backup-directory-alist
           `(("." . ,(expand-file-name
                      (concat emacs-tmp-dir "/backups")))))

          ;; backup every save, instead of just the first time in the buffer
          (defun force-backup-of-buffer ()
            (setq buffer-backed-up nil))
          (add-hook 'before-save-hook  'force-backup-of-buffer)))

;; dl-misc
(use-package dl-misc
  :load-path "lisp/"
  :demand t
  :bind ("C-x C-c" . kill-emacs-no-prompt))

;; dl-simple
(use-package dl-simple
  :load-path "lisp/"
  :demand t ;; load this immediately, even though we bind things
  :bind (("C-c n" . buffer-cleanup)))

;; dockerfile-mode
(use-package-ensure dockerfile-mode)

;; edbi
(use-package-ensure edbi
  :bind ("C-c d" . edbi:open-db-viewer))

;; emacs-lisp-mode
(use-package emacs-lisp-mode
  :init (progn
          ;; pretty-lambdada
          (use-package-ensure pretty-lambdada)

          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
          (add-hook 'emacs-lisp-mode-hook 'pretty-lambda-mode)))

;; executable
(use-package executable
  :init (progn
          (add-hook 'after-save-hook (lambda()
                                       (when (and (buffer-file-name)
                                                  (not (s-ends-with? ".py" (buffer-file-name))))
                                         (executable-make-buffer-file-executable-if-script-p))))))

;; expand-region
(use-package-ensure expand-region
  :bind ("C-q" . er/expand-region))

;; files
(use-package files
  :bind (("C-c z" . prefixed-revert-buffer-all-buffers)
         ("C-x C-r" . rename-current-buffer-file)
         ("C-x C-k" . delete-current-buffer-file))
  :init (progn
          (custom-set-variables
           '(require-final-newline t))

          ;; taken from http://www.emacswiki.org/emacs/RevertBuffer
          (defun revert-all-buffers ()
            "Refreshes all open buffers from their respective files."
            (interactive)
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (and (buffer-file-name)
                           (file-exists-p (buffer-file-name))
                           (not (buffer-modified-p)))
                  (revert-buffer t t t))))
            (message "Refreshed open files."))

          (defun prefixed-revert-buffer-all-buffers(arg)
            "Reverts buffer normally, calls revert-all-buffers if prefix argument
   is present."
            (interactive "P")
            (if arg
                (revert-all-buffers)
              (revert-buffer t t)))

          (defun rename-current-buffer-file ()
            "Renames current buffer and file it is visiting."
            (interactive)
            (let ((name (buffer-name))
                  (filename (buffer-file-name)))
              (if (not (and filename (file-exists-p filename)))
                  (error "Buffer '%s' is not visiting a file!" name)
                (let ((new-name (read-file-name "New name: " filename)))
                  (if (get-buffer new-name)
                      (error "A buffer named '%s' already exists!" new-name)
                    (rename-file filename new-name 1)
                    (rename-buffer new-name)
                    (set-visited-file-name new-name)
                    (set-buffer-modified-p nil)
                    (message "File '%s' successfully renamed to '%s'"
                             name (file-name-nondirectory new-name)))))))

          (defun delete-current-buffer-file ()
            "Removes file connected to current buffer and kills buffer."
            (interactive)
            (let ((filename (buffer-file-name))
                  (buffer (current-buffer))
                  (name (buffer-name)))
              (if (not (and filename (file-exists-p filename)))
                  (ido-kill-buffer)
                (when (yes-or-no-p "Are you sure you want to remove this file? ")
                  (delete-file filename)
                  (kill-buffer buffer)
                  (message "File '%s' successfully removed" filename)))))))

;; find-dired
(use-package find-dired
  :bind ("C-c f" . find-dired))

;; flycheck
(use-package-ensure flycheck)

;; framemove
(use-package-ensure framemove
  :config (progn
            (require 'windmove)
            (require 'cl)
            (windmove-default-keybindings)

            (setq framemove-hook-into-windmove t)))

;; gist
(use-package-ensure gist
  :bind ("C-c h" . actual-gist-region-or-buffer)
  :config (progn
            (defun actual-gist-region-or-buffer(arg)
              (interactive "P")
              (if (region-active-p)
                  (gist-region (region-beginning) (region-end) arg)
                (gist-buffer arg)))))

;; git-link
(use-package git-link
  :bind ("C-c g" . git-link))

;; git-timemachine
(use-package-ensure git-timemachine
  :bind ("C-c t" . git-timemachine))

;; grunt
(use-package-ensure grunt)

;; helm
(use-package-ensure helm
  :demand t
  :bind (("C-c h" . helm-command-prefix)
         ("C-c l" . helm-locate)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-s o" . helm-occur))
  :config (progn
            (require 'helm-config)

            (setq helm-M-x-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-recentf-fuzzy-match t)

            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
            (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

            (when (executable-find "curl")
              (setq helm-google-suggest-use-curl-p t))

            (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                  helm-ff-file-name-history-use-recentf t)
            (helm-mode 1)))

;; helm-dash
(use-package-ensure helm-dash
  :config (progn
            (setq helm-dash-browser-func 'eww
                  helm-dash-common-docsets '("Ansible"
                                             "BackboneJS"
                                             "CMake"
                                             "D3JS"
                                             "Docker"
                                             "ElasticSearch"
                                             "Flask"
                                             "Grunt"
                                             "Jade"
                                             "Jasmine"
                                             "JavaScript"
                                             "Lo-Dash"
                                             "MomentJS"
                                             "Pandas"
                                             "PostgreSQL"
                                             "Python 2"
                                             "Python 3"
                                             "Redis"
                                             "SQLAlchemy"
                                             "Stylus"
                                             "Twisted"
                                             "UnderscoreJS"
                                             "Vagrant"
                                             "jQuery"))

            ;; find all common docsets that aren't installed, install them
            (-map 'helm-dash-install-docset
                  (-difference helm-dash-common-docsets (helm-dash-installed-docsets)))))

;; highlight-symbol
(use-package-ensure highlight-symbol
  :bind ("M-s h s" . highlight-symbol))

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :commands ibuffer
  :init (progn
          (setq ibuffer-saved-filter-groups
                (quote (("default"
                         ("dired" (mode . dired-mode))
                         ("org" (mode . org-mode))
                         ("python" (mode . inferior-python-mode))
                         ("prodigy" (or
                                     (mode . prodigy-mode)
                                     (mode . prodigy-view-mode)))
                         ("terminals" (mode . term-mode))
                         ("magit" (or
                                   (mode . magit-mode)
                                   (mode . magit-log-mode)
                                   (mode . magit-process-mode)
                                   (mode . magit-diff-mode)
                                   (mode . magit-stash-mode)))
                         ("helm" (mode . helm-mode))
                         ("misc" (or
                                  (name . "^\\*scratch\\*$")
                                  (name . "^\\*Messages\\*$")))))))

          ;; switch to the above defined 'default' filter group on ibuffer-mode-hook
          (add-hook 'ibuffer-mode-hook (lambda ()
                                         (ibuffer-switch-to-saved-filter-groups "default")))))


;; ido-ubiquitous
;; puts ido everywhere, including webjump
(use-package-ensure ido-ubiquitous
  :demand t
  :config (ido-ubiquitous-mode 1))

;; jade-mode
(use-package-ensure jade-mode)

;; jasminejs-mode
(use-package-ensure jasminejs-mode
  :bind ("C-c j" . jasminejs-prefix-map)
  :config (progn
            (add-hook 'jasminejs-mode-hook 'jasminejs-add-snippets-to-yas-snippet-dirs)))

;; js2-mode
;; @todo mozrepl, swank, slime, kite?
(use-package-ensure js2-mode
  ;; flycheck does checking for jshint and jscs
  :config (progn
            (custom-set-variables
             '(js2-mode-show-parse-errors nil)
             '(js2-strict-missing-semi-warning nil))

            (add-hook 'js2-mode-hook 'js2-refactor-mode)
            (add-hook 'js2-mode-hook 'rainbow-mode)
            (add-hook 'js2-mode-hook 'company-mode)
            (add-hook 'js2-mode-hook 'tern-mode)
            (add-hook 'js2-mode-hook 'flycheck-mode)

            (add-hook 'js2-mode-hook #'jscs-indent-apply))
  :mode ("\\.js\\'" . js2-mode))

;; js2-refactor
(use-package-ensure js2-refactor)

;; jscs
(use-package-ensure jscs)

;; json-mode
(use-package-ensure json-mode
  :mode ("\\.json\\'" . json-mode))

;; jump-char
(use-package-ensure jump-char
  :bind (("M-n" . jump-char-forward)
         ("M-p" . jump-char-backward))
  :config (bind-key "<return>" 'jump-char-exit jump-char-isearch-map))

;; key-chord
(use-package-ensure key-chord
  :init (key-chord-mode +1)
  :config (progn
            (key-chord-define-global "1n" #'flycheck-next-error)
            (key-chord-define-global "1p" #'flycheck-previous-error)
            (key-chord-define-global "1f" #'flycheck-list-errors)

            (key-chord-define-global "hh" #'highlight-symbol)
            (key-chord-define-global "ww" #'ace-jump-word-mode)
            (key-chord-define-global "jj" #'ace-jump-char-mode)
            (key-chord-define-global "kk" #'ace-jump-line-mode)
            (key-chord-define-global "uu" #'undo-tree-visualize)))

;; lisp-mode
(use-package lisp-mode
  :init (add-hook 'lisp-mode-hook 'eldoc-mode))

;; magit
(use-package-ensure magit
  :bind ("C-x s" . magit-status)
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (add-hook 'git-commit-mode-hook 'flyspell-mode))
  :config (progn
            ;; custom set this?
            (setq magit-completing-read-function 'magit-ido-completing-read)

            (custom-set-variables
             '(magit-push-always-verify nil))

            ;; add --first-parent to the log view
            (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent")

            (defadvice magit-status (around magit-fullscreen activate)
              "Always fullscreen magit."
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (defun magit-quit-session ()
              "Restores the previous window configuration and kills the magit buffer"
              (interactive)
              (kill-buffer)
              (jump-to-register :magit-fullscreen))

            (bind-key "q" 'magit-quit-session magit-status-mode-map)

            (add-hook 'magit-status-mode-hook 'magit-toggle-margin)))

;; menu-bar
(use-package menu-bar
  :bind ("C-x k" . kill-this-buffer))

;; multiple-cursors
(use-package-ensure multiple-cursors
  :bind (("C-c SPC" . set-rectangular-region-anchor)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; multi-term
(use-package-ensure multi-term)

;; newcomment
(use-package newcomment
  :bind ("C-c C-c" . comment-or-uncomment-line-or-region)
  :init (progn
          (defun comment-or-uncomment-line-or-region ()
            "Comments or uncomments the current line or region."
            (interactive)
            (if (region-active-p)
                (comment-or-uncomment-region (region-beginning) (region-end))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))))))

;; org-mode
(use-package org
  :init (progn
          (use-package-ensure org-bullets
            :init (add-hook 'org-mode-hook #'org-bullets-mode))

          (require 'windmove)

          (add-hook 'org-mode-hook #'toggle-truncate-lines)

          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right))
  :commands (org-mode org-agenda org-capture)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("<f12>" . org-agenda))
  :config (progn
            (setq org-replace-disputed-keys t)

            (custom-set-variables
             '(org-enforce-todo-dependencies t)
             '(org-agenda-dim-blocked-tasks t)
             '(org-agenda-files '("~/org" "~/kworg"))
             '(org-agenda-custom-commands
               '(("w" "Work"
                  ((agenda "")
                   (todo "TODO")
                   (todo "ON HOLD"))
                  ((org-agenda-files '("~/kworg" "~/kworg/calendars/dan.lamanna.org"))))
                 ("p" "Personal"
                  ((agenda "")
                   (todo "TODO")
                   (todo "ON HOLD"))
                  ((org-agenda-files '("~/org"))))
                 ("m" "Mixed"
                  ((agenda "")
                   (todo "TODO")
                   (todo "ON HOLD"))
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
             '(org-completion-use-ido t)
             '(org-image-actual-width nil)
             '(org-outline-path-complete-in-steps nil)
             '(org-refile-allow-creating-parent-nodes 'confirm)
             '(org-refile-targets '((org-agenda-files :maxlevel . 8)))
             '(org-todo-keywords '((sequence "TODO(t!)" "ON HOLD(h@)" "|" "DONE(d!)" "INVALID(i@)")))
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

;; org-babel
(use-package org-babel
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((sh . t)
               (emacs-lisp . nil)))))

;; org-gcal
(use-package org-gcal
  :load-path "site-lisp/org-gcal.el/"
  :config (progn
            (use-package-ensure alert)
            (use-package-ensure request-deferred)

            (defun org-gcal-attendance-filter (event)
              "Determine if I am attending `event'. Err on the side of
               caution, so if it can't be determined, leave it there just in case."
              (let* ((attendees (plist-get event :attendees))
                     (self (cl-find-if (lambda (attendee)
                                         (plist-get attendee :self)) attendees)))
                (if (or (not attendees)
                        (not self))
                    t
                  (not (string= "declined"
                                (plist-get self :responseStatus))))))

            ;; org-gcal--notify does all sorts of annoying things with logo images?
            ;; redefine it to not be so awful
            (defun org-gcal--notify (title mes)
              (message (format "%s | %s" title mes)))

            (custom-set-variables
             '(org-gcal-auto-archive nil)
             '(org-gcal-fetch-event-filters '(org-gcal-attendance-filter)))

            (when (file-exists-p "~/.emacs.d/lisp/org-gcal-credentials.el")
              (require 'org-gcal-credentials)
              (run-at-time 0 3600 'org-gcal-fetch))))

;; prodigy
(use-package-ensure prodigy
  :bind ("C-c P" . prodigy)
  :config (progn
            (require 'dl-work-prodigy nil 'noerror)))

(use-package prog-mode
  :init (progn
          (add-hook 'prog-mode-hook 'column-number-mode)))

;; python
(use-package python
  :init (progn
          (use-package-ensure elpy
            :pin elpy
            :commands elpy-mode)

          (add-hook 'python-mode-hook (lambda()
                                        (elpy-mode)
                                        (elpy-use-ipython))))
  :commands python-mode)

;; rainbow-mode
(use-package-ensure rainbow-mode
  :pin gnu)

;; restclient
(use-package-ensure restclient
  :init (progn
          (defun scratch-restclient()
            (interactive)
            (let ((restclient-buf-name "*scratch-restclient*"))
              (with-current-buffer (get-buffer-create restclient-buf-name)
                (restclient-mode)
                (pop-to-buffer restclient-buf-name)))))
  :bind ("C-c v" . scratch-restclient))

;; smartparens
(use-package-ensure smartparens
  :demand t
  :config (progn
            (require 'smartparens-config)
            (smartparens-global-mode)))

;; smart-tab
(use-package-ensure smart-tab
  :init (global-smart-tab-mode 1)
  :config (progn
            (setq smart-tab-using-hippie-expand t)

            (setq hippie-expand-try-functions-list
                  '(yas-hippie-try-expand
                    try-complete-file-name-partially
                    try-expand-all-abbrevs
                    try-expand-dabbrev
                    try-expand-dabbrev-all-buffers
                    try-expand-dabbrev-from-kill)))
  :diminish smart-tab-mode)

;; sphinx-doc
(use-package-ensure sphinx-doc
  :config (progn
            (add-hook 'python-mode-hook 'sphinx-doc-mode)))

;; stylus-mode
(use-package-ensure stylus-mode
  :config (progn
            (add-hook 'stylus-mode-hook 'rainbow-mode)))

;; term/ansi-term
(use-package term
  :init (progn
          (defun ansi-term-zsh()
            (interactive)
            (ansi-term "/bin/zsh")))
  :bind ("C-c q" . ansi-term-zsh)
  :config (progn
            (add-hook 'term-mode-hook (lambda()
                                        (yas-minor-mode -1)))))

;; tern
(use-package-ensure tern)

;; tramp
(use-package tramp
  :config (progn
            (custom-set-variables
             '(tramp-default-method "ssh"))
            '(tramp-auto-save-directory (concat emacs-tmp-dir "/backups")))

  (defun sudo-tramp-current-file()
    "Opens the current buffer-file-name in tramp as sudo."
    (interactive)
    (when buffer-file-name
      (let ((pos (point)))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
        (goto-char pos)))))

;; undo-tree
(use-package-ensure undo-tree
  :demand t ;; load it immediately, undo-tree-visualize isn't autoloaded?
  :config (progn
            (global-undo-tree-mode)

            (custom-set-variables
             '(undo-tree-visualizer-timestamps t)
             '(undo-tree-visualizer-diff t)))
  :diminish undo-tree-mode)

;; uniquify
(use-package uniquify
  :init (progn
          (setq
           uniquify-buffer-name-style 'reverse
           uniquify-separator " - "
           uniquify-after-kill-buffer-p t
           uniquify-ignore-buffers-re "^\\*")))

;; virtualenvwrapper
(use-package-ensure virtualenvwrapper)

;; vlf
(use-package-ensure vlf
  :config (progn
            (require 'vlf-setup)))

;; webjump
(use-package webjump
  :bind ("C-x g" . webjump)
  :init (progn
          (setq webjump-sites
                '(("google"        . [simple-query "google" "www.google.com/search?q=" ""])
                  ("stackoverflow" . [simple-query "stack overflow" "http://stackoverflow.com/search?q=" ""])))))

;; web-mode
;; @todo web-mode-jshint
(use-package-ensure web-mode
  :config (progn
            (add-hook 'web-mode-hook 'rainbow-mode))
  :mode ("\\.html\\'" . web-mode))

;; whitespace
(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

;; windmove
(use-package windmove)

;; winner-mode
(use-package winner
  :config (winner-mode t))

;; yaml-mode
(use-package-ensure yaml-mode)

;; yasnippet
(use-package-ensure yasnippet
  :init (progn
          (setq yas-snippet-dirs
                '("~/.emacs.d/etc/snippets")))
  :config (progn
            (yas-global-mode 1)
            (setq yas-trigger-key "TAB")

            (yas/reload-all)))

;; zencoding-mode
(use-package-ensure zencoding-mode
  :pin melpa
  :bind ("<C-tab>" . zencoding-expand-yas)
  :config (progn
            (add-hook 'web-mode-hook 'zencoding-mode)))

;; todo:
;; visual regexp/steroids
;; bookmarks
;; autoparens and yas-expand issue
;; insert-pair
;; helm-swoop
;; ace-jump-zap/avy?
;; pdf tools
;; openwith
;; elisp eldoc etc
;; flyspell-prog-mode for comments
;; wget file into buffer as contents/scratch buffer?
;; cmake/make on git checkout?
;; per project, defproject?
;; travis integration https://github.com/nlamirault/emacs-travis
;; per project git hooks on checkout, cmake/pip

(use-package-ensure projectile
  :demand t
  :config (progn
            (projectile-global-mode)))

(use-package-ensure helm-projectile
  :demand t
  :config (progn
            (helm-projectile-on)))

(use-package-ensure helm-ag)

(use-package-ensure bpr
  :demand t
  :config (progn
            (defun bootleg-grunt-watch()
              (when (and (boundp 'grunt-dir)
                         (-contains? '("js" "jade" "styl")
                                     (file-name-extension buffer-file-name)))
                (let ((bpr-colorize-output t)
                      (bpr-close-after-success t)
                      (bpr-process-mode #'comint-mode)
                      (default-directory grunt-dir))
                  (bpr-spawn "./node_modules/grunt-cli/bin/grunt --debug-js"))))

            (add-hook 'after-save-hook 'bootleg-grunt-watch)))

;; (use-package-ensure defproject
;;   :demand t
;;   :config (progn
;;             (defproject minerva
;;               :path "/home/dan/projects/minerva-container/girder/plugins/minerva"
;;               :vars ((grunt-dir "/home/dan/projects/minerva-container/girder")))))


(use-package-ensure helm-ctest
  :bind ("M-s t" . helm-ctest))
