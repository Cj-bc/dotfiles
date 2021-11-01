
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

(defun ol-notes-link-to-path (link)
  "Convert notes: link to actual path.
Accept link without `notes:'

(eq (ol-notes-link-to-path (ol-notes-path-to-link l))
    l)
"
    (string-match ol-notes-path-syntax link)
    (let ((organization (match-string 1 link))
	  (class (match-string 2 link))
	  (year (or (match-string 3 link) (nth 2 (calendar-current-date))))
	  )
      (format "%s/%s/%s/%s.org" ol-notes-root-dir organization class year)
    ))

(defun ol-notes-path-to-link (path)
  "Convert path to notes: link if possible. Return `nil' if it is
invalid path

Accept link without `notes:'
"
  (string-match (rx  (group (+? (not "/"))) "/" (group (+? (not "/"))) "/" (group (repeat 4 digit)) "\.org" eos) path)
  (let ((organization (match-string 1 path))
	(class (match-string 2 path))
	(year (match-string 3 path))
	)
    (when (and organization class year)
      (concat (format "%s/%s:%s" organization class year)))))


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
  (let* ((target-files (file-expand-wildcards (concat ol-notes-root-dir "/**/**/*.org")))

	 (completion-answer (completing-read "<org>/<class>:<year>: "
					     (seq-map (lambda (p) (ol-notes-path-to-link p)) target-files)))
	 )
    (concat (format "notes:%s" completion-answer)
	     )))

(provide 'ol-notes)
