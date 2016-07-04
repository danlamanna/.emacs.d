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
