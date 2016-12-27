(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Setup tmp dirs
(setq emacs-tmp-dir (expand-file-name "~/.emacs.d/tmp"))
(setq emacs-autosave-dir (concat emacs-tmp-dir "/autosaves/"))
(setq recentf-save-file (concat emacs-tmp-dir "/" "recentf"))

(use-package f
  :ensure t)

;; Set/load custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(if (f-exists? custom-file)
    (load custom-file))


(use-package browse-url
  :config (progn
            (setq browse-url-browser-function 'browse-url-generic
                  browse-url-generic-program (or (executable-find "google-chrome")
                                                 (executable-find "chromium")
                                                 (executable-find "firefox")))))

(use-package cmake-mode
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode))

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c o" . crux-open-with)
         ("C-c u" . crux-view-url)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c I" . crux-find-user-init-file)))

(use-package delsel
  :config (pending-delete-mode t))

(use-package dl-backups
  :load-path "lisp/")

(use-package dl-dired
  :load-path "lisp/")

(use-package dl-docker
  :load-path "lisp/")

(use-package dl-helm
  :load-path "lisp/")

(use-package dl-javascript
  :load-path "lisp/")

(use-package dl-lisp
  :load-path "lisp/")

(use-package dl-misc
  :load-path "lisp/")

(use-package dl-navigation
  :load-path "lisp/")

(use-package dl-org
  :load-path "lisp/")

(use-package dl-python
  :load-path "lisp/")

(use-package dl-theme
  :load-path "lisp/")

(use-package elfeed-org
  :ensure t
  :config (progn
            (use-package elfeed
              :ensure t)

            (use-package elfeed-goodies
              :ensure t
              :config (progn
                        (elfeed-goodies/setup)))

            (setq rmh-elfeed-org-files '("~/etc/elfeed.org"))
            (elfeed-org)))

(use-package dl-tramp
  :load-path "lisp/")

(use-package dl-web
  :load-path "lisp/")

;; ediff
;; expand everything that can be expanded before ediffing
;; this is useful for org-mode and code folding
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-highlight-all-diffs 'nil))

(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

(use-package files
  :bind ("C-c z" . revert-buffer)
  :config (progn
            (custom-set-variables
             '(require-final-newline t))))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package git-timemachine
  :ensure t)

(use-package github-browse-file
  :ensure t
  :config (progn
            (custom-set-variables
             '(github-browse-file-show-line-at-point t))))

(use-package jinja2-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package jump-char
  :ensure t
  :bind (("M-n" . jump-char-forward)
         ("M-p" . jump-char-backward))
  :config (progn
            (bind-key "<return>" 'jump-char-exit jump-char-base-map)))

(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config (progn
            (key-chord-define-global "hh" 'helm-dash)
            (key-chord-define-global "ww" 'ace-jump-word-mode)
            (key-chord-define-global "jj" 'ace-jump-char-mode)
            (key-chord-define-global "kk" 'ace-jump-line-mode)
            (key-chord-define-global "uu" 'undo-tree-visualize)))

(use-package magit
  :ensure t
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

(use-package multiple-cursors
  :ensure t
  :bind (("C-c SPC" . set-rectangular-region-anchor)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package nginx-mode
  :ensure t)

(use-package paren
  :init (add-hook 'prog-mode-hook #'show-paren-mode)
  :config (progn
            (custom-set-variables
             '(show-paren-style 'mixed))))

;; Show column number, use camelcase with word navigation
(use-package prog-mode
  :init (progn
          (add-hook 'prog-mode-hook 'column-number-mode)
          (add-hook 'prog-mode-hook 'subword-mode)))

(use-package projectile
  :ensure t
  :config (progn
            (setq projectile-known-projects-file
                  (concat emacs-tmp-dir "/projectile-bookmarks.eld"))
            (projectile-global-mode)))

(use-package replace
  :bind ("C-c r" . replace-string))

(use-package simple
  :bind (("M-g" . goto-line)
         ("C-z" . quoted-insert)
         ("C-c n" . buffer-cleanup))
  :config (progn
            (custom-set-variables
             '(save-interprogram-paste-before-kill t))

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

(use-package undo-tree
  :ensure t
  :config (progn
            (custom-set-variables
             '(undo-tree-visualizer-timestamps t)
             '(undo-tree-visualizer-diff t))

            (global-undo-tree-mode))
  :diminish undo-tree-mode)

(use-package uniquify
  :config (custom-set-variables
           '(uniquify-buffer-name-style 'reverse)
           '(uniquify-separator " - ")
           '(uniquify-after-kill-buffer-p t)
           '(uniquify-ignore-buffers-re "^\\*")))

(use-package vc
  :config (progn
            (custom-set-variables
             '(vc-follow-symlinks t))))

(use-package vlf
  :ensure t
  :config (progn
            (require 'vlf-setup)))

(use-package whitespace
  :bind ("C-c w" . whitespace-mode))

(use-package winner
  :config (winner-mode t))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (progn
            (custom-set-variables
             '(yas-snippet-dirs "~/.emacs.d/etc/snippets"))

            (yas-global-mode 1)
            (yas-reload-all)))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(put 'narrow-to-region 'disabled nil)
(setq inhibit-splash-screen t)

(defadvice Man-quit(after maybe-close-frame activate)
  "`delete-frame-on-man-quit' is defined by an external script which
   launches an emacs frame for a specific man page. After being launched,
   this allows the `Man-quit' function to close the frame."
  (if delete-frame-on-man-quit
      (delete-frame)))

(bind-key "M-o" 'other-window)
