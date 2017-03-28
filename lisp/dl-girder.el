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

(provide 'dl-girder)
