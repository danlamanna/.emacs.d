(require 'grep)

(setq geojs-project-dir "~/projects/geojs")
(setq geojs-build-dir "~/projects/geojs-build")
(setq geojs-venv "geojs-source")

(add-to-list 'safe-local-variable-values
             '(compile-command . (format "cd %s && ctest" geojs-build-dir)))

(prodigy-define-service
  :name "GeoJS Grunt"
  :command "grunt"
  :cwd "/home/dan.lamanna/projects/geojs"
  :args '("watch")
  :on-output (lambda (&rest args)
               (let ((output (plist-get args :output))
                     (service (plist-get args :service)))
                 (when (s-matches? "Done, without errors" output)
                   (message "GeoJS Built."))))
  :init (lambda ()
          (venv-workon geojs-venv))
  :tags '(geojs grunt))

(dir-locals-set-class-variables 'geojs
                                `((nil . ((compile-command .
                                                           (format "cd %s && ctest" geojs-build-dir))
                                          (grep-find-ignored-files . ,(append '("geo.js"
                                                                                "geo.min.js")
                                                                              grep-find-ignored-files))))
                                  (python-mode . ((eval . ,(venv-workon geojs-venv))))))

(if (file-exists-p geojs-project-dir)
    (dir-locals-set-directory-class geojs-project-dir 'geojs))

(provide 'geojs)
