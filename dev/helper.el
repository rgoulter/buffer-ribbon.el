;;; helper.el --- dev helper methods when hacking buffer-ribbon.el  -*- lexical-binding:t -*-

;;; commentary:

;;; code:

;;;; "dummy" buffer methods

(defun buffer-ribbon/--dummy-buffer-with-number (&optional num)
  "Make a dummy buffer.

The buffer will have contents NUM.
For package development purposes."
  (let ((buf (generate-new-buffer "*ribbon-dummy*"))
        (num (or num 2)))
    (with-current-buffer  buf
      (princ num buf)
      (text-scale-set 13)
      (set-buffer-modified-p (buffer-modified-p)))))

(defun buffer-ribbon/--make-dozen-dummy-buffers ()
  "Make 12 dummy buffers.

For package development purposes."
  (dotimes (i 12)
           (buffer-ribbon/--dummy-buffer-with-number i)))

;;; helper.el ends here
