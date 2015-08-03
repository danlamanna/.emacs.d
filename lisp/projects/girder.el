(setq girder-project-dir "~/projects/girder")
(setq girder-build-dir "~/projects/girder-build")
(setq girder-venv "girder-source")

(prodigy-define-service
  :name "Girder Dev"
  :command "python"
  :cwd girder-project-dir
  :args '("-m" "girder")
  :init (lambda ()
          (venv-workon girder-venv))
  :tags '(girder))

(prodigy-define-service
  :name "Girder Dev Grunt"
  :command "grunt"
  :cwd girder-project-dir
  :args '("watch" "--debug-js")
  :init (lambda ()
          (venv-workon girder-venv))
  :tags '(girder grunt))

(dir-locals-set-class-variables 'girder
                                `((nil . ((compile-command . (format "cd %s && ctest -j4" girder-build-dir))))
                                  (js2-mode . ((flycheck-jshintrc . ,(concat girder-project-dir "/tests/jshint.cfg"))
                                               (flycheck-jscsrc . ,(concat girder-project-dir "/tests/jsstyle.cfg"))))
                                  (python-mode . ((eval . ,(venv-workon girder-venv))
                                                  (flycheck-python-flake8-executable . "/usr/bin/flake8")
                                                  (flycheck-flake8rc . ,(concat girder-project-dir "tests/flake8.cfg"))))))

(if (file-exists-p girder-project-dir)
    (dir-locals-set-directory-class girder-project-dir 'girder))

(provide 'girder)
