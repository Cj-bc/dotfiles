(in-package #:nyxt-user)

(defun set-url-to (new-url)
  "Set current buffer's url to given url."
  (let ((target-buffer (current-buffer)))
    (setf (slot-value target-buffer 'url)
	  (quri:uri new-url))
    (reload-buffers (list target-buffer))))

(define-command-global modify-url-with-external-editor ()
  "Modify current buffer's url by using external editor"
  (let* ((old-url (slot-value (current-buffer) 'url))
	 (new-url (nyxt::%edit-with-external-editor (quri:render-uri old-url))))
    (set-url-to new-url)))
