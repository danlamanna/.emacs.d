(defun find-girder-dir()
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (and (f-exists? (f-join dir "girder"))
                                 (f-exists? (f-join dir "setup.py"))))))

(dir-locals-set-class-variables 'girder-dir-locals
      '((nil . ((eval . (set (make-local-variable 'project-path) (find-girder-dir)))))
        (js2-mode . ((eval . (nvm-use-for))
                     (eval . (set (make-local-variable 'flycheck-javascript-eslint-executable)
                                  (f-join project-path "node_modules/.bin/eslint")))
                     (eval . (set (make-local-variable 'flycheck-eslintrc)
                                  (f-join project-path ".eslintrc")))
                     (js-indent-level . 4)))
        (python-mode . ((eval . (set (make-local-variable 'flycheck-flake8rc)
                                     (f-join project-path "tests/flake8.cfg")))))))

(defun girder-combine-coverage(&rest args)
  "Combine coverage in a Girder project.
   If this is called from within a Girder project, a coverage executable
   exists, and uncombined coverage files exist (.coverage.*). This will
   combine the coverage files into a .coverage file."
  (-when-let* ((default-directory (find-girder-dir))
               (coverage-executable (executable-find "coverage"))
               (coverage-files (f-glob ".coverage.*")))
    (shell-command (format "%s combine" coverage-executable))))

(advice-add 'pycoverage-mode :before 'girder-combine-coverage)

(provide 'dl-girder)
