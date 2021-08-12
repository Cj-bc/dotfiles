(defcustom blog-post-dir-path "~/blog"
  "Directory that contains all blog posts")

(defcustom blog-post-format "org"
  "Format of blog post"
  :type '(choice (const "org") (const "md")))

(defun blog--current-date ()
  "Get current date in (year month day) format"
  (let ((year (car (reverse (calendar-current-date))))
	(month-date (reverse (cdr (reverse (calendar-current-date))))))
    (seq-concatenate 'list (list year) month-date)))

(defun blog-get-new-post-name ()
  "Return new post name. This function will use mini buffer"
  (let* ((date (string-join (seq-map '(lambda (n) (format "%02d" n))
				     (blog--current-date))
			    "-"))
	 (slug (replace-regexp-in-string "\s" "-"
					 (read-string "slug?: "))))
    (concat blog-post-dir-path "/"
	    (string-join (list date slug) "-")
	    "." "org")
    ))
  
(defun blog-visit-new-post ()
  "Create new post name and visit it. This is made for org-capture"
  (interactive)
  (find-file (blog-get-new-post-name))
  (goto-char (point-min)))
 

(provide 'blog)
