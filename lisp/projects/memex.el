(setq memex-project-dir "~/projects/memex")
(setq memex-venv "memex-tempus")

(prodigy-define-service
  :name "Memex Tempus"
  :command "python"
  :cwd (concat memex-project-dir "/smqtk/python/smqtk/web/geospace/src")
  :args '("main.py")
  :init (lambda ()
          (venv-workon memex-venv))
  :tags '(memex smqtk))

(prodigy-define-service
  :name "Memex Notebooks"
  :command "ipython"
  :cwd (concat memex-project-dir "/notebooks")
  :args '("notebook" "--no-browser" "--ip=0.0.0.0" "--port=8889")
  :init (lambda ()
          (venv-workon memex-venv))
  :tags '(memex ipython)
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(provide 'memex)
