(prodigy-define-service
  :name "Girder Dev"
  :command "python"
  :cwd (concat (expand-file-name "~") "/projects/girder")
  :args '("-m" "girder")
  :init (lambda ()
          (venv-workon "girder-source"))
  :tags '(girder))

(prodigy-define-service
  :name "Girder Dev Grunt"
  :command "grunt"
  :cwd (concat (expand-file-name "~") "/projects/girder")
  :args '("watch" "--debug-js")
  :init (lambda ()
          (venv-workon "girder-source"))
  :tags '(girder grunt))

(prodigy-define-service
  :name "GeoJS Grunt"
  :command "grunt"
  :cwd "/home/dan.lamanna/projects/geojs"
  :args '("watch")
  :init (lambda ()
          (venv-workon "geojs-source"))
  :tags '(geojs grunt))

(prodigy-define-service
  :name "Memex Tempus"
  :command "python"
  :cwd (concat (expand-file-name "~") "/projects/memex/smqtk/python/smqtk/web/geospace/src")
  :args '("main.py")
  :init (lambda ()
          (venv-workon "memex-tempus"))
  :tags '(memex smqtk))

(prodigy-define-service
  :name "Memex Notebooks"
  :command "ipython"
  :cwd (concat (expand-file-name "~") "/projects/memex/notebooks")
  :args '("notebook" "--no-browser" "--ip=0.0.0.0" "--port=8889")
  :init (lambda ()
          (venv-workon "memex-tempus"))
  :tags '(memex ipython)
  :kill-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "XData Notebooks"
  :command "ipython"
  :cwd (concat (expand-file-name "~") "/projects/xdata")
  :args '("notebook" "--no-browser" "--ip=0.0.0.0" "--port=8888")
  :tags '(xdata ipython))

(provide 'dl-work-prodigy)
