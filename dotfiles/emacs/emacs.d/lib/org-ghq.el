(require 'ol)

(org-link-set-parameters "ghq"
			 :follow #'org-ghq-open
			 ; :export #'org-ghq-export
			 )

; WIP
(defun org-ghq--run-shell-command (cmd &rest args)
  "Run shell command and return its stdout"
  (with-temp-buffer 
    (eval (append '(call-process) (list cmd) '(nil nil nil) args))
    (remove ?\n (buffer-string))
    )
  )

(defun org-ghq-open (name _)
  "Visit the ghq directory"
  (setq ghq-project-path (org-ghq--run-shell-command "bash" "-c" (concat "ghq list | grep " name "| tr -d '\n'")))
  (unless (eq "" ghq-project-path)
    (let* ((ghq-root-path (org-ghq--run-shell-command "ghq" "root"))
	  (ghq-full-path (concat ghq-root-path "/" ghq-project-path)))
      (dired (expand-file-name ghq-full-path))))
      )
  

; ; WIP
; (defun org-ghq-export (link description format _)
;   "Export a ghq project path from org files.
;    It will link to the 'remote' url if available"

(provide 'org-ghq)
