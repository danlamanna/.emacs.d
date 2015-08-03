(setq xdata-project-dir "~/projects/xdata")
(setq xdata-venv "xdata")

(prodigy-define-service
  :name "XData Notebooks"
  :command "ipython"
  :cwd xdata-project-dir
  :args '("notebook" "--no-browser" "--ip=0.0.0.0" "--port=8888")
  :tags '(xdata ipython))

(provide 'xdata)
