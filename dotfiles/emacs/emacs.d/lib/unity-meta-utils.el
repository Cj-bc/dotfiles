;;; unity-meta-utils.el --- Unity meta file and scene files utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <the_cat@VWP>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;; Scene file thingatpt

;; Unityのシーンファイル・プレファブに表れるローカルのfileIDは、1行の中で書かれるという前提を置いている
(put 'unity/local-fileid-reference 'end-op
     (lambda ()
       (progn (beginning-of-line) (re-search-forward "{fileID: [0-9]+}" (line-end-position) t))))

(put 'unity/local-fileid-reference 'beginning-op
     (lambda ()
       (progn (end-of-line) (re-search-backward "{fileID: [0-9]+}" (line-beginning-position) t))))

(defun unity/--xref-backend () 'unity)

;; WIP
;; (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'unity))))

;; (cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'unity)))
;;   (let ((bounds (bounds-of-thing-at-point 'unity/local-fileid-reference)))
;;     (and bounds
;; 	 (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;; (cl-defmethod xref-backend-definitions ((_backend (eql 'unity)) identifier)
;;   (save-match-data
;;     (when (string-match "^{fileID: ([0-9]+)}$" identifier)
;;       (goto-char (point-min))
;;       (when (re-search-forward (format "^--- !u![0-9]+ &%s" (match-string 1)))
;; 	(xref-make (format "Location for fileID: %s" (match-string 1)) (point)))
;;       )))

(provide 'unity-meta-utils)
;;; unity-meta-utils.el ends here
