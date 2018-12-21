;;; test-helper.el --- Helpers for buffer-ribbon-test.el

;;; Commentary:

;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"))

;; My experience: Debian on WSL can't run make-frame in -batch,
;; (make-frame also doesn't work in MSYS2 shell, or CMD shell;
;;  although set-frame-size doesn't work in emacs -batch run from
;;  MSYS2 shell or CMD shell either).
;; To run tests headlessly with -batch, I can accept using the
;;  selected frame, but I'd like to use a separate frame when
;;  running the ERT tests from a w32 session.
(defun buffer-ribbon/run-with-test-frame (body)
  "Call BODY in an appropriate frame."
  (if (and (eq 'w32 (window-system))
           (not (tty-type)))
    (let ((test-frame (make-frame)))
      (unwind-protect
        (with-selected-frame test-frame (funcall body test-frame))
        (delete-frame test-frame)))
    (progn
      ;; tty's frame starts 10x10 which
      ;; isn't large enough for splitting into 3x2.
      (set-frame-size (selected-frame) 188 48)
      (let ((old-window-config (current-window-configuration)))
        (funcall body (selected-frame))
        (set-window-configuration old-window-config)))))

;;; test-helper.el ends here
