(setq geojs-project-dir "~/projects/geojs")
(setq geojs-venv "geojs-source")

(prodigy-define-service
  :name "GeoJS Grunt"
  :command "grunt"
  :cwd "/home/dan.lamanna/projects/geojs"
  :args '("watch")
  :init (lambda ()
          (venv-workon geojs-venv))
  :tags '(geojs grunt))

(dir-locals-set-class-variables 'geojs
                                `((python-mode . ((eval . ,(venv-workon geojs-venv))))))

(if (file-exists-p geojs-project-dir)
    (dir-locals-set-directory-class geojs-project-dir 'geojs))

(provide 'geojs)
