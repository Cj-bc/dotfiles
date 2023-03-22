(defun org-count-words/subtree ()
  "`count-words-region' for org subtree"
  (interactive)
  (eval `(count-words--message "Subtree" ,@(org-count-words/--subtree-region))))

(defun org-count-words/--subtree-region ()
  "Return `(BEGINING-OF-SUBTREE END-OF-SUBTREE)'
It does NOT include heading line."
    (save-excursion
      (save-match-data
	;; Tbh, I don't know why I need `org-with-limited-levels'.
	;; I added it because 
	(org-with-limited-levels 
	 `(,(progn (org-back-to-heading t) (next-line)
		   (while (or (org-at-drawer-p) (org-at-property-p))
		     (org-forward-element))
		   (point))
	   ,(progn (org-end-of-subtree t t)
	       (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
	       (point)))))))

(defvar org-count-words/property-name "WORDS_LIMIT")

(defun )
