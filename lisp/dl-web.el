(use-package rainbow-mode
  :pin gnu)

(use-package stylus-mode
  :init (add-hook 'stylus-mode-hook 'rainbow-mode))

(use-package css-mode
  :init (add-hook 'css-mode-hook 'rainbow-mode))

(use-package web-mode
  :init (add-hook 'web-mode-hook 'rainbow-mode))

(use-package jade-mode)

(provide 'dl-web)
