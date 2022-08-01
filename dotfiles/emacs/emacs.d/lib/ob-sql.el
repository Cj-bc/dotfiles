;;; ob-sql.el --- org babel for SQL -*- lexical-binding: t; -*-
;;; Commentary:


;; Copyright (C) 2022 Cj.bc-sd a.k.a Cj-bc

;; Author: Cj.bc-sd a.k.a Cj-bc 
;; Created: 6 Jul 2022
;; Keywords: processes
;; URL: https://github.com/Cj-bc/dotfiles
;; Package-Requires: (org sql)

;; This file is not part of GNU Emacs.

;; This file is free software...
...
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(require 'ob)
(require 'sql)

(defun org-babel-execute:sql (body params)
  "Execute a block of SQL code with Babel.


Parameters:

`:sql-type' -- Specify which SQL executable to use.
Currently, only `'mysql' is supported.
`:user' -- Login user. defaults to `sql-user'.
`:password' -- Login password. defaults to `sql-password'.
`:database' -- Database to operate on. defaults to `sql-database'.
`:executable' -- Executable to use. defaults to `sql-mysql-program'.
"
  (pcase (cdr (assq :sql-type params))
    ('mysql (ob-sql/mysql/execute body params))
    (_ (error "Currently 'mysql' is only option"))
    ))

(defun ob-sql/mysql/execute (body params)
  "Execute `body' SQL string in MySQL, and returns result.

For parameters, see `org-babel-execute:sql'
"
  (let ((user (or (cdr (assq :user params)) sql-user))
	(password (or (cdr (assq :password params)) sql-password))
	  (database (or (cdr (assq :database params)) sql-database))
	  (executable (or (cdr (assq :executable params)) sql-mysql-program))
	  )
    (org-babel-eval (format "%s --user %s %s" executable user database) body)))

(provide 'ob-sql)
;;; ob-sql.el ends here
