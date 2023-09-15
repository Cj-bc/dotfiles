;;; wat-ts-mode.el --- WASM Text format mode with tree-sitter support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Cj-bc

;; Author:   Cj-bc
;; Keywords: languages
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.0"))
;; Created: Sep 2023

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

;; Yet another major mode for WAT(Wasm Text format)
;; Main reason I'm writing this because
;;
;; + Wanted to avoid handcrafting syntax because it's not stable yet.
;;
;; Alternatives
;; + https://github.com/wasm-lsp/wasm-mode :: not updated since 2020
;; + https://github.com/devonsparks/wat-mode :: Active, but without tree-sitter

;;; Code:
(require 'treesit)

(defvar wat-ts-mode-imenu-settings
  '("Import" "import" ))
(define-derived-mode wat-ts-mode lisp-mode "WebAssemby-text"
  (when (treesit-ready-p 'wat)
    (setq-local treesit-font-lock-settings
		(treesit-font-lock-rules
		 :feature 'comment
		 :language 'wat
		 '((comment_line) @font-lock-comment-face)

		 :feature 'string
		 :language 'wat
		 '((string) @font-lock-string-face)

		 :feature 'definition
		 :language 'wat
		 '((identifier) @font-lock-function-name-face)

		 :feature 'type
		 :language 'wat
		 '((value_type) @font-lock-type-face)

		 :feature 'keyword
		 :language 'wat
		 '((["func" "export" "param" "result"]
		    @font-lock-keyword-face))

		 :feature 'builtin
		 :language 'wat
		 '(([op_const] @font-lock-builtin-face))
		 ))
    (setq-local treesit-font-lock-feature-list
		'((comment definition)
		  (comment definition keyword string type)))

    (setq-local treesit-simple-imenu-settings
		'(("*Import*" "module_field_func"
		   #'(lambda (node)
		       (treesit-query-capture node '((import) @capture)))
		   #'(lambda (node)
		       (format "%v" node))
		       ;; (seq-reduce
		       ;; 	#'(lambda (acc cap) (format "%s.%s" acc (treesit-node-text cap)))
		       ;; 	(treesit-query-capture node '((name _ @name)) nil nil t)
		       ;; 	""))
		   )))
    (treesit-major-mode-setup))
  )

(provide 'wat-ts-mode)
;;; wat-ts-mode.el ends here
