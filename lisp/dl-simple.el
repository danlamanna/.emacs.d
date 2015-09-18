(require 'dash)

(use-package simple
  :ensure nil
  :init (progn
          (custom-set-variables
           '(next-line-add-newlines t))

          (defun back-to-indentation-or-beginning ()
            (interactive)
            (if (bolp)
                (back-to-indentation)
              (beginning-of-line))))
  :bind (("M-g" . goto-line)
         ("C-a" . back-to-indentation-or-beginning)))

;; coding standards
;; both these lists should be lowercased
(setq no-cleanup-filenames '("makefile" "rules"))
(setq no-cleanup-extensions '("md" "org" "xml" "tsv" "csv" "config" "conf" "rst"))

(defun should-cleanup-buffer?()
  "Returns t if the buffer is an actual file, the files extension isn't in no-cleanup-extensions,
and it's name isn't in no-cleanup-filenames."
  (and (buffer-file-name)
       (not (-contains? no-cleanup-filenames (downcase (file-name-nondirectory (buffer-file-name)))))
       (not (and (file-name-extension (buffer-file-name)) ;has a file extension
                 (-contains? no-cleanup-extensions (downcase (file-name-extension (buffer-file-name))))))))

(defun buffer-cleanup()
  "A less safe buffer cleanup, indents everything."
  (interactive)
  (buffer-cleanup-safe)
  (indent-region (point-min) (point-max)))

(defun buffer-cleanup-safe()
  (interactive)
  (when (should-cleanup-buffer?)
    (whitespace-cleanup)
    (untabify (point-min) (point-max))
    (set-buffer-file-coding-system 'utf-8)))

(add-hook 'before-save-hook 'buffer-cleanup-safe)

(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))


(fset 'yes-or-no-p 'y-or-n-p)

(provide 'dl-simple)
