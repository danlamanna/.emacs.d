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

(provide 'dl-backups)
