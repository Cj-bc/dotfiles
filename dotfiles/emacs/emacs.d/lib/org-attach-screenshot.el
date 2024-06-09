;;; org-attach-screenshot.el --- Take screeenshot and attach it directly to org file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Cj-bc a.k.a Cj.bc-sd

;; Author:  Cj-bc a.k.a Cj.bc-sd
;; Keywords: docs

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
(require 'org-attach)

(defcustom org-attach-screenshot/screenshot-template

  )
(defun org-attach-screenshot/attach-screenshot ()
    "Take screenshot and put them into attachment directory"
    (let ((temp-file-name (format-time-string  "org-attach-screenshot--%Y-%m-%d--%H-%M-%S.png")))
      (call-process "~/.local/bin/screenshot" nil nil nil "-s" (concat "/tmp/" temp-file-name))
      (org-attach-attach (concat "/tmp/" temp-file-name) nil 'mv)
      temp-file-name
      ))

(defun org-attach-screenshot/insert (&optional description)
  "Take screenshot, attach it to current org file, and insert link to it in buffer."
  (interactive "MDscription(Enter for no desc): ")
  (when (eq major-mode 'org-mode)
    (let ((filename (org-attach-screenshot/attach-screenshot))
  	  (template (if (string-empty-p description)  "[[attachment:%s]]" "[[attachment:%s][%s]]"))
	  )
      (insert (format template filename description)))))


(provide 'org-attach-screenshot)
;;; org-attach-screenshot.el ends here
