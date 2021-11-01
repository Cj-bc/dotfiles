;;; ol-notes.el --- External link to manage note files -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Cj.bc-sd a.k.a Cj-bc

;; Author: Cj.bc-sd a.k.a Cj-bc <cj.bc-sd@outlook.jp>
;; Created: 1 Nov 2021
;; URL: https://github.com/Cj-bc/dotfiles.git

;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
	 ;; those codes below is copied from help:pcomplete/org-mode/searchhead
	 (titles (with-temp-buffer (find-file (ol-notes-link-to-path completion-answer))
				   (save-excursion
				     (goto-char (point-min))
				     (let (tbl)
				       (while (re-search-forward org-outline-regexp nil t)
					 ;; Remove the leading asterisk from
					 ;; `org-link-heading-search-string' result.
					 (push (substring-no-properties (org-link-heading-search-string) 1) tbl))
				       (pcomplete-uniquify-list tbl)))))
	 (title (completing-read "Title(optional): " titles))
	 )
    (eval `(concat (format "notes:%s" completion-answer)
		   ,@(when title '("#" title)))
	     )))

(provide 'ol-notes)
