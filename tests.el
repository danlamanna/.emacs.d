;; (ert-deftest test-python-flycheck-overlay ()
;;   (with-python-temp-buffer
;;       "import os\n"
;;       (should (not (eql (executable-find "flake8") nil)))
;;       (message "%s" (buffer-substring-no-properties (point-min) (point-max)))
;;       (flycheck-error-list-refresh)
;;       (goto-char (point-min))
;;       (message "%s" (flycheck-next-error))

;;       (should (= (length (overlays-in (point-min) (point-max))) 2))))


;; Taken from emacs-jedi test-jedi.el
(defmacro with-python-temp-buffer (code &rest body)
  "Insert `code' and enable `python-mode'. cursor is beginning of buffer"
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq python-indent-guess-indent-offset nil)
     (insert ,code)
     (goto-char (point-min))
     (python-mode)
     (font-lock-fontify-buffer)
     ,@body))

(ert-deftest test-python-modes ()
  (with-python-temp-buffer
    "" ;; empty buffer
    (should (eql (bound-and-true-p jedi-mode) t))
    (should (eql (bound-and-true-p flycheck-mode) t))
    (should (eql (bound-and-true-p flymake-mode) nil))))


(ert-deftest test-python-yasnippet ()
  (with-python-temp-buffer
    "ifm"
    (goto-char (point-max))
    (should (eql (bound-and-true-p yas-global-mode) t))
    (call-interactively (key-binding [C-tab]))
    (should (s-equals? (s-trim (buffer-substring-no-properties (point-min) (point-max)))
                 "if __name__ == '__main__':"))))

(ert-deftest test-camel-case-words ()
  "When I am in prog-mode (or any derived mode), word keybindings
should operate on a camelCase basis."
  (with-temp-buffer
    (progn
      (prog-mode)
      (insert "someCamelCaseWord")
      (goto-char (point-min))
      (call-interactively (key-binding "\M-d"))
      (should (string-equal (buffer-string) "CamelCaseWord")))))
