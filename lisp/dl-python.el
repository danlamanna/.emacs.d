(defun dl-jedi-setup()
  "Sets up jedi to use, disables company-mode since jedi
prefers auto-complete and it's easier not to argue with it."
  (custom-set-variables
   '(jedi:complete-on-dot t)
   '(jedi:tooltip-method nil)
   '(jedi:use-shortcuts t))
  
   (jedi:setup)
   (company-mode -1))

(use-package sphinx-doc
  :init (add-hook 'python-mode-hook 'sphinx-doc-mode))

(provide 'dl-python)
