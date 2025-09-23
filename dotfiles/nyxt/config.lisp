(in-package #:nyxt-user)
(define-nyxt-user-system :nyxt-user/base-config
  :component ("util.lisp" "search-engines.lisp"))


(define-configuration buffer
    ((default-modes (append '(nyxt/mode/vi:vi-normal-mode) %SLOT-VALUE%))
     (override-map
      (let ((map (make-keymap "override-map")))
	(define-key map
	    "C-x" 'nothing
	    "C-x b" 'switch-buffer
	  )))))

(define-configuration prompt-buffer
  ((default-modes (reduce #'(lambda (acc mode) (remove mode acc)) '(nyxt/mode/vi:vi-insert-mode
								    nyxt/mode/vi:vi-normal-mode)
			  :initial-value %SLOT-VALUE%))
   (keyscheme nyxt/keyscheme:emacs)))


(define-configuration browser
    ((external-editor-program '("emacsclient" "-c" "--frame-parameters" "((name . \"nyxt.edit-url\"))"))))
