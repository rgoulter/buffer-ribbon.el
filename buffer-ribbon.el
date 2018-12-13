;; splitting vertically ('below) first, then horizontally
;; means that other-window navigates "right" across
;; the top, then "right" across the bottom.
(defun buffer-ribbon/split-into-3-2-v-h ()
  (interactive)
  (let ((bottom-window (split-window nil nil 'below)))
    (split-window nil nil 'right)
    (split-window nil nil 'right)
    (split-window bottom-window nil 'right)
    (split-window bottom-window nil 'right)
    (balance-windows)))

;; splitting horizontally ('right) first means that
;; other-window goes down, then returns to the top/right,
;; then down.
;; I think this is a nicer way of thinking about
;; a "ribbon" of buffers.
(defun buffer-ribbon/split-into-3-2 ()
  (interactive)
  (let* ((mid-window (split-window nil nil 'right))
         (right-window (split-window mid-window nil 'right)))
    (split-window nil nil 'below)
    (split-window mid-window nil 'below)
    (split-window right-window nil 'below)
    (balance-windows)))

(defun buffer-ribbon/--dummy-buffer-with-number (&optional num)
  (interactive)
  (let ((buf (generate-new-buffer "*ribbon-dummy*"))
        (num (or num 2)))
    (with-current-buffer  buf
      (princ num buf)
      (text-scale-set 13)
      (set-buffer-modified-p (buffer-modified-p)))))

(defun buffer-ribbon/--make-dozen-dummy-buffers ()
  (interactive)
  (dotimes (i 12)
    (buffer-ribbon/--dummy-buffer-with-number i)))

(provide 'buffer-ribbon)

