(setq romanesco-project-dir "~/projects/romanesco")
(setq romanesco-build-dir "~/projects/romanesco/build")
(setq romanesco-venv "romanesco")

(add-to-list 'safe-local-variable-values
             '(compile-command . (format "cd %s && ctest -j4" romanesco-build-dir)))
(add-to-list 'safe-local-variable-values
             '(flycheck-python-flake8-executable . "/usr/bin/flake8"))

(dir-locals-set-class-variables 'romanesco
                                `((nil . ((eval . ,(setenv "PYTHONPATH"
                                                           (concat
                                                            (expand-file-name "~/projects/VTK/build/Wrapping/Python") ":"
                                                            (expand-file-name "~/projects/VTK/build/lib") ":"
                                                            (getenv "PYTHONPATH"))))
                                          (compile-command . (format "cd %s && ctest -j4" romanesco-build-dir))))
                                  (python-mode . ((eval . ,(venv-workon romanesco-venv))
                                                  (flycheck-python-flake8-executable . "/usr/bin/flake8")
                                                  (flycheck-flake8rc . ,(concat romanesco-project-dir "/setup.cfg"))))))

(if (file-exists-p romanesco-project-dir)
    (dir-locals-set-directory-class romanesco-project-dir 'romanesco))

(provide 'romanesco)
