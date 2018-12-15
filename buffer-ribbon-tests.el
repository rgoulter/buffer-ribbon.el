;;; buffer-ribbon-tests.el --- Tests buffer-ribbon with ERT

;;; Commentary:

;; ERT tests for buffer-ribbon.el

;;; Code:

(require 'ert)
(require 'buffer-ribbon)

(ert-deftest buffer-ribbon/test-make-buffer-ribbon ()
  (should-not (equal nil
                     (buffer-ribbon/make-buffer-ribbon))))

(defun buffer-ribbon/kludge-use-blank-global-vars (body)
  "kludge to allow test which uses the ribbon/patch grid
which is currently a global variable."
  (let ((old-buffer-ribbon buffer-ribbon/global-ribbon)
        (old-patch-grid buffer-ribbon/global-patch-grid))
  (unwind-protect
      (progn (setq buffer-ribbon/global-ribbon nil)
             (setq buffer-ribbon/global-patch-grid nil)
             (funcall body))
    (progn (setq buffer-ribbon/global-ribbon old-buffer-ribbon)
           (setq buffer-ribbon/global-patch-grid old-patch-grid)))))

(ert-deftest buffer-ribbon/test-e2e-from-existing-shift-right-shift-left ()
  (buffer-ribbon/kludge-use-blank-global-vars
   (lambda ()
     ;; ASSEMBLE
     ;;; make a frame
     (let ((test-frame (make-frame)))
       (with-selected-frame test-frame
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
           (buffer-ribbon/shift-right)
           ;;; set the two 'new' grid buffers
           ;;; (since we shifted 'right' & started in (0,0),
           ;;;  need to replace (0,0) and (0,1)):
           (set-window-buffer nil (get-buffer-create "7"))
           (other-window +1)
           (set-window-buffer nil (get-buffer-create "8"))
           (other-window -1)
           ;;; shift back to the left
           (buffer-ribbon/shift-left)
           ;; ASSERT
           ;;; check that the buffers are the same as we set them
           (let ((actual-patch-grid-buffers (mapcar 'window-buffer
                        (buffer-ribbon/patch-grid-windows (buffer-ribbon/current-patch-grid)))))
           (should (equal old-patch-grid-buffers
                          actual-patch-grid-buffers))
           (delete-frame test-frame))))))))

(ert-deftest buffer-ribbon/test-e2e-from-existing-shift-left-shift-right ()
  (buffer-ribbon/kludge-use-blank-global-vars
   (lambda ()
     ;; ASSEMBLE
     ;;; make a frame
     (let ((test-frame (make-frame)))
       (with-selected-frame test-frame
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
           (buffer-ribbon/shift-left)
           ;;; set the two 'new' grid buffers
           ;;; (since we shifted 'left' & started in (0,0),
           ;;;  need to replace (2,0) and (2,1)):
           (other-window +4)
           (set-window-buffer nil (get-buffer-create "7"))
           (other-window +1)
           (set-window-buffer nil (get-buffer-create "8"))
           (other-window -5)
           ;;; shift back to the right
           (buffer-ribbon/shift-right)
           ;; ASSERT
           ;;; check that the buffers are the same as we set them
           (let ((actual-patch-grid-buffers (mapcar 'window-buffer
                        (buffer-ribbon/patch-grid-windows (buffer-ribbon/current-patch-grid)))))
           (should (equal old-patch-grid-buffers
                          actual-patch-grid-buffers))
           (delete-frame test-frame))))))))


(provide 'buffer-ribbon-tests)

;;; buffer-ribbon-tests.el ends here
