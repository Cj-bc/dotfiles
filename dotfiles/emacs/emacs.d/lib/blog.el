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
  "Return new post name based on user input"
  (let ((post-slug (read-string "slug?: ")))
    (blog-get-new-post-name--append-date-and-prefix post-slug)))
  
(defun blog-get-new-post-name--append-date-and-prefix (title)
  "Return new post name for given title. This function "
  (let* ((date (blog--current-date))
	 (post-date-string (string-join (seq-map 'number-to-string date) "-"))
	 (post-file-name (string-join (list (string-join (list post-date-string title) "-") "org") ".")) ;; 一度的に対応しなきゃなの苦手
	)
    (string-join (list blog-post-dir-path post-file-name) "/")))
  
