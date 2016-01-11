(require 'f)



(defvar .emacs.d-support-path
  (f-dirname load-file-name))

(defvar .emacs.d-features-path
  (f-parent .emacs.d-support-path))

(defvar .emacs.d-root-path
  (f-parent .emacs.d-features-path))

(add-to-list 'load-path .emacs.d-root-path)

(load (f-join .emacs.d-root-path "init.el"))
(require 'espuds)
(require 'ert)

(And "^I activate \"\\(.+\\)\"$"
     (lambda (some-mode)
       (funcall (intern some-mode))))

(Then "^I should have \"\\(.+\\)\" overlays$"
      (lambda (expected-num-overlays)
	(let ((expected-num-overlays (string-to-number expected-num-overlays))
	      (actual-num-overlays (length (overlays-in (point-min) (point-max)))))
	  (cl-assert (flycheck-has-current-errors-p) nil
		   "Expected %d overlays, but found %d." expected-num-overlays actual-num-overlays))))

(Then "^\"\\(.+\\)\" should be active$"
      (lambda (some-mode)
	(cl-assert (bound-and-true-p (intern some-mode)) nil
		   "Expected %s to be active." some-mode)))


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
