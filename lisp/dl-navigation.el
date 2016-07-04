(use-package ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode-pop-mark)
  :commands (ace-jump-word-mode
             ace-jump-char-mode
             ace-jump-line-mode))


(provide 'dl-navigation)
