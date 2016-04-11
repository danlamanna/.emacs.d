(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(require 'use-package)

(setq emacs-tmp-dir (expand-file-name "~/.emacs.d/tmp"))
(setq emacs-autosave-dir (concat emacs-tmp-dir "/autosaves/"))
(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (f-exists? custom-file)
    (load custom-file))

(use-package ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode-pop-mark)
  :commands (ace-jump-word-mode
             ace-jump-char-mode
             ace-jump-line-mode))

(use-package company
  :config (global-company-mode))

(use-package delsel
  :config (pending-delete-mode t))

;; @todo dired-async
(use-package dired
  :config (progn
            (custom-set-variables
             '(dired-listing-switches "-Alh")
             '(dired-dwim-target t)
             '(dired-recursive-copies 'always)
             '(dired-recursive-deletes 'always)
             '(wdired-allow-to-change-permissions t))

            ;; Taken from whattheemacsd.com
            (defun dired-back-to-top ()
              (interactive)
              (beginning-of-buffer)
              (dired-next-line 2))

            (define-key dired-mode-map
              (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

            (defun dired-jump-to-bottom ()
              (interactive)
              (end-of-buffer)
              (dired-next-line -1))

            (define-key dired-mode-map
              (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

;; I mostly just use this for font-locking keywords
(use-package dockerfile-mode)

(use-package elfeed-org
  :config (progn
            (use-package elfeed
              :config (progn
                        (custom-set-variables
                         '(elfeed-search-filter "@1-week-ago +unread"))))
            (use-package elfeed-goodies
              :config (progn
                        (elfeed-goodies/setup)))

            (setq rmh-elfeed-org-files '("~/etc/elfeed.org"))
            (elfeed-org)))


(use-package flycheck
  :config (global-flycheck-mode))

(use-package lisp-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)))

(use-package expand-region
  :bind ("C-q" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-c SPC" . set-rectangular-region-anchor)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package prog-mode
  :init (progn
          (add-hook 'prog-mode-hook 'column-number-mode)
          (add-hook 'prog-mode-hook 'subword-mode)))



(use-package uniquify
  :config (custom-set-variables
           '(uniquify-buffer-name-style 'reverse)
           '(uniquify-separator " - ")
           '(uniquify-after-kill-buffer-p t)
           '(uniquify-ignore-buffers-re "^\\*")))

(use-package vlf
  :config (progn
            (require 'vlf-setup)))



(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

(use-package windmove)

(use-package yaml-mode)

(use-package dl-org
  :load-path "lisp/")

(use-package dl-mail
  :load-path "lisp/")


(use-package dl-python
  :load-path "lisp/"
  :demand t
  :init (use-package jedi
          :init (add-hook 'python-mode-hook 'dl-jedi-setup))
  :commands python-mode)

(use-package dl-web
  :load-path "lisp/"
  :demand t)



(use-package term
  :init (progn
          (defun ansi-term-zsh-or-bash()
            (interactive)
            (ansi-term (or (executable-find "zsh")
                           (executable-find "bash")))))
  :bind ("C-c q" . ansi-term-zsh-or-bash))


(use-package winner
  :config (winner-mode t))

(use-package jump-char
  :bind (("M-n" . jump-char-forward)
         ("M-p" . jump-char-backward))
  :config (bind-key "<return>" 'jump-char-exit jump-char-isearch-map))

(use-package key-chord
  :init (key-chord-mode 1)
  :config (progn
            (key-chord-define-global "ww" 'ace-jump-word-mode)
            (key-chord-define-global "jj" 'ace-jump-char-mode)
            (key-chord-define-global "kk" 'ace-jump-line-mode)
            (key-chord-define-global "uu" 'undo-tree-visualize)))

(use-package magit
  :bind ("C-x s" . magit-status)
  :init (add-hook 'git-commit-mode-hook 'flyspell-mode)
  :config (progn
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

            (bind-key "q" 'magit-quit-session magit-status-mode-map)))

(use-package prodigy
  :bind ("C-c P" . prodigy))



(use-package helm
  :demand t
  :bind (("C-c h" . helm-command-prefix)
         ("C-c l" . helm-locate)
         ("C-c f" . helm-find)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-s o" . helm-occur))
  :init (progn
          (require 'helm-config)

          (custom-set-variables
           '(helm-M-x-fuzzy-match t)
           '(helm-buffers-fuzzy-matching t)
           '(helm-recent-fuzzy-match t)
           '(helm-split-window-in-side-p t)
           '(helm-ff-file-name-history-use-recentf t))

            (helm-mode))
  :config (progn
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))

(use-package undo-tree
  :config (progn
            (custom-set-variables
             '(undo-tree-visualizer-timestamps t)
             '(undo-tree-visualizer-diff t))

            (global-undo-tree-mode))
  :diminish undo-tree-mode)

(use-package projectile
  :demand t
  :config (projectile-global-mode))

(use-package helm-projectile
  :demand t
  :init (helm-projectile-on))

(use-package helm-ag)


;; (use-package crux
;;   :demand t
;;   :config (progn
;;          (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)))

(use-package browse-url
  :config (progn
            (setq browse-url-browser-function 'browse-url-generic
                  browse-url-generic-program (or (executable-find "google-chrome")
                                                 (executable-find "chromium")
                                                 (executable-find "firefox")))))

(use-package simple
  :bind (("M-g" . goto-line)
         ("C-z" . quoted-insert)
         ("C-c n" . buffer-cleanup))
  :config (progn
            (defadvice zap-to-char (after zap-until-char (arg char) activate)
              "Makes zap-to-char act like zap-until-char."
              (insert char)
              (backward-char 1))

            ;; cleanup buffer on save
            ;; both these lists should be lowercased
            (setq no-cleanup-filenames '("makefile" "rules"))
            (setq no-cleanup-extensions '("md" "org" "xml" "tsv" "csv" "config" "conf" "rst"))

            (defun should-cleanup-buffer?()
              "Returns t if the buffer is an actual file, the files extension isn't in no-cleanup-extensions,
and it's name isn't in no-cleanup-filenames."
              (and (buffer-file-name)
                   (not (-contains? no-cleanup-filenames (downcase (file-name-nondirectory (buffer-file-name)))))
                   (not (and (file-name-extension (buffer-file-name)) ;has a file extension
                             (-contains? no-cleanup-extensions (downcase (file-name-extension (buffer-file-name))))))))

            (defun buffer-cleanup()
              "A less safe buffer cleanup, indents everything."
              (interactive)
              (buffer-cleanup-safe)
              (indent-region (point-min) (point-max)))

            (defun buffer-cleanup-safe()
              (interactive)
              (when (should-cleanup-buffer?)
                (whitespace-cleanup)
                (untabify (point-min) (point-max))
                (set-buffer-file-coding-system 'utf-8)))

            (add-hook 'before-save-hook 'buffer-cleanup-safe)))

(use-package replace
  :bind ("C-c r" . replace-string))

(use-package paren
  :init (add-hook 'prog-mode-hook #'show-paren-mode)
  :config (progn
            (custom-set-variables
             '(show-paren-style 'mixed))))

(use-package smartparens
  :demand t
  :config (smartparens-global-mode))

(use-package yasnippet
  :config (progn
            (custom-set-variables
             '(yas-snippet-dirs "~/.emacs.d/etc/snippets"))

            (yas-global-mode 1)
            (yas-reload-all)))

;; related to built-ins
(fset 'yes-or-no-p 'y-or-n-p)

;; aesthetics related
;; use of fbound for cross-platform compat (x and nox)
(-map (lambda(f)
        (if (fboundp f)
            (funcall f -1))) '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(custom-set-faces
 '(default ((t (:family "Source Code Pro"
                        :height 100)))))


                        ;; misc
(defun insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

(global-set-key "\M-'" 'insert-quotations)
(global-set-key "\M-\"" 'insert-quotes)

;; backspace starts the isearch over
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; backups
(custom-set-variables
 '(auto-save-interval 30)
 '(auto-save-list-file-prefix emacs-autosave-dir)
 '(auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))
 '(vc-make-backup-files t) ;; backup version controlled files, too
 '(backup-by-copying t) ;; no symlinks
 '(delete-old-versions t) ;; no confirm
 '(kept-new-versions 20)
 '(kept-old-versions 20)
 '(version-control t) ;; number backups
 '(backup-directory-alist
   `(("." . ,(expand-file-name
              (concat emacs-tmp-dir "/backups"))))))

;; backup every save, instead of just the first time in the buffer
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)

(use-package files
  :bind ("C-c C-z" . revert-buffer)
  :config (progn
            (custom-set-variables
             '(require-final-newline t))))
(put 'narrow-to-region 'disabled nil)

(add-hook 'jade-mode-hook 'highlight-indentation-mode)
(setq highlight-indentation-offset 2)
