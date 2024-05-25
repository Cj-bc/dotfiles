(in-package #:nyxt-user)
(define-nyxt-user-system :nyxt-user/base-config
  :component ("util.lisp" "search-engines.lisp"))


(define-configuration buffer
    ((default-modes (append '(vi-normal-mode dark-mode) %SLOT-VALUE%))
     (override-map
      (let ((map (make-keymap "override-map")))
	(define-key map
	    "C-x" 'nothing
	    "C-x b" 'switch-buffer
	    "d" 'delete-buffer
	    )
	))))

(define-configuration prompt-buffer
  ((default-modes (pushnew 'vi-insert-mode %SLOT-VALUE%))
   (keyscheme nyxt/keyscheme:emacs)))


(define-configuration browser
    ((external-editor-program '("emacsclient" "-c" "--frame-parameters" "((name . \"nyxt.edit-url\"))"))))
