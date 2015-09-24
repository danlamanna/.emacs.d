;; remove clutter...
(use-package paren
  :init (add-hook 'prog-mode-hook #'show-paren-mode)
  :config (custom-set-variables
           '(show-paren-style 'mixed)))

(setq default-frame-alist '((cursor-color . "white")
                            (scroll-bar-mode . -1)))

(menu-bar-mode -1)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(defun toggle-fullscreen()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(use-package-ensure monokai-theme
  :demand t
  :init (load-theme 'monokai))

(toggle-fullscreen)

(provide 'dl-aesthetics)
