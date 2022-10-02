(define-configuration buffer
    ((default-modes (append '(vi-normal-mode dark-mode) %slot-default%))
     (override-map
      (let ((map (make-keymap "override-map")))
	(define-key map
	    "C-x" 'nothing
	    "C-k b" 'switch-buffer)
	))))


(define-configuration base-mode
    ((keymap-scheme
      (define-scheme (:name-prefix "my-base" :import %slot-default%)
	  scheme:vi-normal
	(list "C-d" 'nyxt/web-mode:scroll-page-down
	      )
	))
     ))
