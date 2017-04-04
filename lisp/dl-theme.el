(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t)
  :config (progn
            (eval-after-load "magit"
              (custom-set-faces
               '(diff-refine-added ((t (:inherit diff-refine-change :background "#2d872d"))))
               '(diff-refine-removed ((t (:inherit diff-refine-change :background "#872c2c"))))))))

(provide 'dl-theme)
