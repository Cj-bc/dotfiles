(defcustom org-diary-directory "~/Diary"
  "Directory that contains all diary entries"
  )

(defun org-diary-date-to-path (date)
  "Convert given time to the diary entry path"
  (format "%s/%s" org-diary-directory (format-time-string "%Y-%m-%d.org" date))
  )

(defun org-diary-visit-date (date &optional dest)
  "Open specific date"
  (find-file (org-diary-date-to-path date))
  (let ((position (case dest
		    ('memo (org-find-exact-headline-in-buffer "Memo" nil t))
		    (nil (point-min))
		    )))
    (goto-char (or position (point-min)))
    ))

(defun org-diary-visit-today (&optional dest)
  "Open today's diary"
  (interactive)
  (org-diary-visit-date (current-time) dest)
  )

(defun org-diary-visit-yesterday (&optional dest)
  "Open yesterday's diary"
  (interactive)
  (org-diary-visit-date (time-add nil (days-to-time -1)) dest)
  )

(provide 'org-diary)
