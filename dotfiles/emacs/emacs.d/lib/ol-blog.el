(require 'blog)
(require 'ol)

(org-link-set-parameters "blog"
			 :follow #'org-blog-open
			 ; :export org-blog-export
			 ; :store org-blog-store
			 :complete #'org-blog-complete
			 )
			 
(defun org-blog-open (path arg)
  "Opens blog post"
  (org-link-open-as-file (concat blog-post-dir-path "/" path ".org") arg)
  )

(defun org-blog-complete (&optional arg)
  "Completion for blog post"
  (concat "blog:" (completing-read "blog: "
				   (seq-map '(lambda (s) (substring s 0 -4))
					    (directory-files blog-post-dir-path nil "\.org$"))))
  )

(provide 'ol-blog)
