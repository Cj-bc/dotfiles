(load (nyxt-init-file "./util.lisp"))
(load-after-system :nx-search-engines (nyxt-init-file "search-engines.lisp"))

(in-package #:nyxt-user)

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
   (keymap-scheme scheme:emacs)))



(define-configuration browser
    ((external-editor-program '("emacsclient" "-c" "--frame-parameters" "((name . \"nyxt.edit-url\"))"))))
