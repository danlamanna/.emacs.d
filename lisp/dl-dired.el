(use-package async
  :ensure t
  :init (add-hook 'dired-mode-hook 'dired-async-mode))

(use-package dired
  :config (progn
            (custom-set-variables
             '(dired-clean-up-buffers-too nil)
             '(dired-dwim-target t)
             '(dired-listing-switches "-Alh")
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

(provide 'dl-dired)
