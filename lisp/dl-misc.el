(defun kill-emacs-no-prompt()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; backspace starts the isearch over
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;; misc keybindings
(bind-key "C-z" 'quoted-insert)
(bind-key "C-c r" 'replace-string)
(bind-key "M-o" 'other-window)
(bind-key "C-<return>" 'open-line-below)
(bind-key "C-S-<return>" 'open-line-above)

(provide 'dl-misc)
