;; vagrant tramp
(use-package vagrant-tramp
  :ensure t)


;; docker tramp
(use-package docker-tramp
  :ensure t
  :config (progn
            (custom-set-variables
             '(docker-tramp-use-names t))))

(provide 'dl-tramp)

;; Taken from
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
