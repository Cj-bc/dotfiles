(load (nyxt-init-file "./util.lisp"))

(in-package #:nyxt-user)

(define-configuration buffer
    ((default-modes (append '(vi-normal-mode dark-mode) %slot-default%))
     (override-map
      (let ((map (make-keymap "override-map")))
	(define-key map
	    "C-x" 'nothing
	    "C-k b" 'switch-buffer)
	))))

(define-configuration prompt-buffer
    ((default-modes (append '(vi-insert-mode) %slot-default%))))


(define-configuration base-mode
    ((keymap-scheme
      (define-scheme (:name-prefix "my-base" :import %slot-default%)
	  scheme:vi-normal
	(list "C-d" 'nyxt/web-mode:scroll-page-down
	      "C-u" 'nyxt/web-mode:scroll-page-up
	))
      )))

(define-configuration browser
    ((external-editor-program '("emacsclient" "-c" "--frame-parameters" "((name . \"nyxt.edit-url\"))"))))
