
; notes:your-org/class-name:2021#title

(require 'ol)

(org-link-set-parameters "notes"
			 :follow #'ol-notes-open
			 ; :export #'ol-notes-export
			 :complete #'ol-notes-complete
			 )

(defcustom ol-notes-path-syntax
  (rx (group (+ print)) ?/ (group (+ (not (any "#:"))))
      (optional ?: (group (+ digit)))
      (optional ?# (group (+ print)))
      )
  "regex to parse notes path"
  )
(defcustom ol-notes-root-dir "~/Documents/Note"
  "root path of notes")



(defun ol-notes-open (link arg)
  "Open note file"
  (string-match ol-notes-path-syntax link)
  (let ((organization (match-string 1 link))
	(class-name (match-string 2 link))
	(year (or (match-string 3 link) (nth 2 (calendar-current-date))))
	(title (if (match-string 4 link) (format "::%s" (match-string 4 link)) ""))
	)
    (org-link-open-as-file
     (format "%s/%s/%s/%s.org%s" ol-notes-root-dir organization class-name year title) arg)
    ))


(defun ol-notes-complete (&optional arg)
  "Completion for ol-notes"
  (let* ((parse-filename (rx  (group (+? (not "/")) "/" (+? (not "/"))) "/" (group (repeat 4 digit)) "\.org" eos))
	 (target-files (file-expand-wildcards (concat ol-notes-root-dir "/**/**/*.org")))

	 (completion-answer (completing-read "<org>/<class>:<year>: "
					     (seq-map (lambda (p)
							(progn
							  (string-match parse-filename p)
							  (concat (match-string 1 p) ":"
								  (match-string 2 p)))
							) target-files)))
	 )
    (concat (format "notes:%s" completion-answer)
	     )))

(provide 'ol-notes)
