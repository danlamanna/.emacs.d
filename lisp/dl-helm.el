(use-package helm
  :ensure t
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

(use-package helm-ag
  :ensure t)

(use-package helm-dash
  :ensure t
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
                                             "Jinja"
                                             "MomentJS"
                                             "Pandas"
                                             "PostgreSQL"
                                             "Python_2"
                                             "Python_3"
                                             "Redis"
                                             "SQLAlchemy"
                                             "Stylus"
                                             "Twisted"
                                             "UnderscoreJS"
                                             "Vagrant"
                                             "jQuery"))

            (helm-dash-install-docset "Python 2")

            ;; find all common docsets that aren't installed, install them
            (-map 'helm-dash-install-docset
                  (-difference helm-dash-common-docsets (helm-dash-installed-docsets)))
            ))


(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(provide 'dl-helm)
