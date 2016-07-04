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

(use-package sphinx-doc
  :ensure t
  :init (add-hook 'python-mode-hook 'sphinx-doc-mode))

(use-package virtualenvwrapper
  :ensure t)


(provide 'dl-python)
