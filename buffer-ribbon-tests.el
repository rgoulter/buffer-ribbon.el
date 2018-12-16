;;; buffer-ribbon-tests.el --- Tests buffer-ribbon with ERT

;;; Commentary:

;; ERT tests for buffer-ribbon.el

;;; Code:

(require 'cl)  ;; for caddr
(require 'dash)
(require 'ert)

(require 'buffer-ribbon)

(ert-deftest buffer-ribbon/test-make-buffer-ribbon ()
  (should-not (equal nil
                     (buffer-ribbon/make-buffer-ribbon))))

;; My experience: Debian on WSL can't run make-frame in -batch,
;; (make-frame also doesn't work in MSYS2 shell, or CMD shell;
;;  although set-frame-size doesn't work in emacs -batch run from
;;  MSYS2 shell or CMD shell either).
;; To run tests headlessly with -batch, I can accept using the
;;  selected frame, but I'd like to use a separate frame when
;;  running the ERT tests from a w32 session.
(defun buffer-ribbon/run-with-test-frame (body)
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

(ert-deftest buffer-ribbon/test-e2e-from-existing-scroll-right-scroll-left ()
  ;; ASSEMBLE
  ;;; make a frame
  (buffer-ribbon/run-with-test-frame
    (lambda (test-frame)
      ;;; split into 3x2
      (buffer-ribbon/split-into-3-2)
      ;;; set each of them to distinct buffers
      (dotimes (i 6)
        (set-window-buffer nil (get-buffer-create (int-to-string (+ 1 i))))
        (other-window +1))
      ;;; now init from existing grid
      (buffer-ribbon/init-from-current-windows)
      (let ((old-patch-grid-buffers
             (mapcar 'window-buffer
                     (buffer-ribbon/patch-grid-windows (buffer-ribbon/current-patch-grid)))))
        ;; ACT
        ;;; shift to the right
        (buffer-ribbon/scroll-buffer-ribbon-right)
        ;;; set the two 'new' grid buffers
        ;;; (since we shifted 'right' & started in (0,0),
        ;;;  need to replace (0,0) and (0,1)):
        (set-window-buffer nil (get-buffer-create "7"))
        (other-window +1)
        (set-window-buffer nil (get-buffer-create "8"))
        (other-window -1)
        ;;; shift back to the left
        (buffer-ribbon/scroll-buffer-ribbon-left)
        ;; ASSERT
        ;;; check that the buffers are the same as we set them
        (let ((actual-patch-grid-buffers (mapcar 'window-buffer
                     (buffer-ribbon/patch-grid-windows (buffer-ribbon/current-patch-grid)))))
        (should (equal old-patch-grid-buffers
                       actual-patch-grid-buffers)))))))

(ert-deftest buffer-ribbon/test-e2e-from-existing-scroll-left-scroll-right ()
  ;; ASSEMBLE
  ;;; make a frame
  (buffer-ribbon/run-with-test-frame
    (lambda (test-frame)
      ;;; split into 3x2
      (buffer-ribbon/split-into-3-2)
      ;;; set each of them to distinct buffers
      (dotimes (i 6)
        (set-window-buffer nil (get-buffer-create (int-to-string (+ 1 i))))
        (other-window +1))
      ;;; now init from existing grid
      (buffer-ribbon/init-from-current-windows)
      (let ((old-patch-grid-buffers
             (mapcar 'window-buffer
                     (buffer-ribbon/patch-grid-windows (buffer-ribbon/current-patch-grid)))))
        ;; ACT
        ;;; shift to the left
        (buffer-ribbon/scroll-buffer-ribbon-left)
        ;;; set the two 'new' grid buffers
        ;;; (since we shifted 'left' & started in (0,0),
        ;;;  need to replace (2,0) and (2,1)):
        (other-window +4)
        (set-window-buffer nil (get-buffer-create "7"))
        (other-window +1)
        (set-window-buffer nil (get-buffer-create "8"))
        (other-window -5)
        ;;; shift back to the right
        (buffer-ribbon/scroll-buffer-ribbon-right)
        ;; ASSERT
        ;;; check that the buffers are the same as we set them
        (let ((actual-patch-grid-buffers (mapcar 'window-buffer
                     (buffer-ribbon/patch-grid-windows (buffer-ribbon/current-patch-grid)))))
        (should (equal old-patch-grid-buffers
                       actual-patch-grid-buffers)))))))

(provide 'buffer-ribbon-tests)

;;; buffer-ribbon-tests.el ends here
