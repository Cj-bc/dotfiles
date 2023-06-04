(load (nyxt-init-file "./util.lisp"))
(load-after-system :nx-search-engines (nyxt-init-file "search-engines.lisp"))

(in-package #:nyxt-user)

(define-configuration buffer
    ((default-modes (append '(vi-normal-mode dark-mode) %slot-default%))
     (override-map
      (let ((map (make-keymap "override-map")))
	(define-key map
	    "C-x" 'nothing
	    "C-x b" 'switch-buffer
	    "d" 'delete-buffer
	    )
	))))

(define-configuration prompt-buffer
  ((default-modes (append '(vi-insert-mode) %slot-default%))
   (keymap-scheme scheme:emacs)))


(define-configuration base-mode
    ((keymap-scheme
      (define-scheme (:name-prefix "my-base" :import %slot-default%)
	  scheme:vi-normal
	(list "C-d" 'nyxt/web-mode:scroll-page-down
	      "C-u" 'nyxt/web-mode:scroll-page-up
	      "C-e" 'modify-url-with-external-editor
	))
      )))

(define-configuration browser
    ((external-editor-program '("emacsclient" "-c" "--frame-parameters" "((name . \"nyxt.edit-url\"))"))))

(push #p"~/.emacs.d/elpa/sly-20230411.1523/slynk/" asdf:*central-registry*)
(asdf:load-system :slynk)

(load (nyxt-init-file "my-slynk.lisp"))
