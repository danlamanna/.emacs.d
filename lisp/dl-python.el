(use-package jedi
  :ensure t
  :commands python-mode
  :init (progn
          (defun python-jedi-hook()
            "Sets up jedi to use, disables company-mode since jedi
             prefers auto-complete and it's easier not to argue with it."
            (jedi:setup)
            (company-mode -1))

          (add-hook 'python-mode-hook 'python-jedi-hook))
  :config (progn
            (custom-set-variables
             '(jedi:complete-on-dot t)
             '(jedi:tooltip-method nil)
             '(jedi:use-shortcuts t))))
(use-package importmagic
  :ensure t
  :load-path "~/projects/importmagic.el"
  :bind (:map python-mode-map
              ("C-c i" . importmagic-fix-imports))
  :init (add-hook 'python-mode-hook 'importmagic-mode))

(use-package pycoverage
  :ensure t
  :commands pycoverage-mode)

(use-package realgud
  :ensure t
  :load-path "~/projects/realgud"
  :bind ("C-c d" . dl-realgud-listen)
  :config (progn
            (defun dl-realgud-listen()
              "Scan debugger to be listening using netcat, when done
               call `realgud:pdb-remote' to connect."
              (interactive)
              (async-start (lambda()
                             (save-window-excursion
                               (shell-command "until nc -z localhost 4444; do sleep 1; done")))
                           (lambda(&rest args)
                             (realgud:pdb-remote "telnet localhost 4444"))))))

(use-package sphinx-doc
  :ensure t
  :init (add-hook 'python-mode-hook 'sphinx-doc-mode))

(use-package virtualenvwrapper
  :ensure t)


(provide 'dl-python)
