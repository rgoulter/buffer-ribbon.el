;;;; window grid methods

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

;;;; "dummy" buffer methods

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

;;;; buffer ribbon methods

(defun buffer-ribbon/make-buffer-ribbon (&optional buffers column-height)
  (list 'buffer-ribbon
        (or buffers
            (buffer-ribbon/empty-buffers 6))
        (or column-height 2)))

(defun buffer-ribbon/buffer-ribbon-p (o)
  (and (listp o)
       (eq 'buffer-ribbon (car o))
       (listp (cadr o))
       (integerp (caddr o))))

(defun buffer-ribbon/buffer-ribbon-buffers (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (cadr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/set-buffer-ribbon-buffers (ribbon new-buffers)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (setcdr ribbon
              (cons new-buffers
                    (list (caddr ribbon))))
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/buffer-ribbon-height (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (caddr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/current-buffer-ribbon ()
  (buffer-ribbon/patch-grid-buffer-ribbon
   (buffer-ribbon/current-patch-grid)))

(defun buffer-ribbon/empty-buffer ()
  "function which returns a buffer to assign
as a 'default buffer' when moving the buffer-ribbon
past its defined"
  (get-buffer-create "*scratch*"))

(defun buffer-ribbon/empty-buffers (n)
  (mapcar (lambda (_) (buffer-ribbon/empty-buffer))
          (number-sequence 1 n)))

(defun buffer-ribbon/list-replace-at-offset (old-list offset new-content)
  (let* ((len (length new-content))
         (new-head (-take offset old-list))
         (new-tail (-drop (+ offset len) old-list)))
          (append new-head
                  new-content
                  new-tail)))

(defun buffer-ribbon/buffer-ribbon-width (buffer-ribbon)
  (let ((buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
        (column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (/ (length buffers) column-height)))

(defun buffer-ribbon/column-to-offset (buffer-ribbon column)
  (let ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (* column column-height)))

(defun buffer-ribbon/offset-to-column (buffer-ribbon offset)
  (let ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (/ offset column-height)))

(defun buffer-ribbon/buffer-ribbon-append-column (buffer-ribbon)
  (let* ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (add-buffers (buffer-ribbon/empty-buffers column-height))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon (append old-buffers add-buffers))))

(defun buffer-ribbon/buffer-ribbon-prepend-column (buffer-ribbon)
  (let* ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (add-buffers (buffer-ribbon/empty-buffers column-height))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon (append add-buffers old-buffers))))

;;;; patch grid methods

(defvar buffer-ribbon/global-patch-grid nil
  "use buffer-ribbon/current-patch-grid function instead of accessing this directly")

(defun buffer-ribbon/set-patch-grid-window-parameters (window patch-grid-position)
  (set-window-parameter window 'is-patch-grid t)
  (set-window-parameter window 'patch-grid-position patch-grid-position))

(defun buffer-ribbon/patch-grid-window-p (window)
  (window-parameter window 'is-patch-grid))

(defun buffer-ribbon/patch-grid-window-position (window)
  (window-parameter window 'patch-grid-position))

(defun buffer-ribbon/make-patch-grid (buffer-ribbon windows)
  (let ((patch-grid (list 'patch-grid
                          buffer-ribbon
                          0
                          windows)))
    ;; assign parameters to each of the window
    (-each-indexed windows
                   (lambda (index window)
                     (buffer-ribbon/set-patch-grid-window-parameters window index)))
    patch-grid))

(defun buffer-ribbon/patch-grid-p (o)
  (and (listp o)
       (eq 'patch-grid (car o))
       (buffer-ribbon/buffer-ribbon-p (cadr o))
       (integerp (caddr o))
       (listp (cadddr o))))

(defun buffer-ribbon/patch-grid-buffer-ribbon (patch-grid)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (cadr patch-grid)
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-buffer-ribbon (patch-grid new-buffer-ribbon)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons new-buffer-ribbon
                    (list (caddr patch-grid)
                          (cadddr patch-grid))))
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/patch-grid-column (patch-grid)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (caddr patch-grid)
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-grid-column (patch-grid new-column)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons (cadr patch-grid)
                    (list new-column
                          (cadddr patch-grid))))
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/patch-grid-windows (patch-grid)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (cadddr patch-grid)
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-grid-windows (patch-grid new-windows)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons (cadr patch grid)
                    (caddr patch grid)
                    (list new-windows)))
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/current-patch-grid ()
  buffer-ribbon/global-patch-grid)

(defun buffer-ribbon/patch-grid-width (&optional patch-grid)
  "number of columns the patch grid has"
  3)

(defun buffer-ribbon/patch-grid-height (&optional patch-grid)
  "number of rows the patch grid has"
  2)

(defun buffer-ribbon/patch-grid-count (&optional patch-grid)
  "number of tiles the patch grid has"
  (* (buffer-ribbon/patch-grid-width)
     (buffer-ribbon/patch-grid-height)))

;; this is useful because (window-list) returns
;; in an order I might not like
;;
;; this function returns in an order of columns I like
;; for windows which are horizontal splits of vert splits
;; (or vert splits of horizontal splits)
(defun buffer-ribbon/list-of-windows-in-ribbon-order (&optional win)
  (let ((win (or win (car (window-tree)))))
    (if (windowp win)
      (list win)
      (let* ((is-vert-split (car win))
             (children-wins (cddr win))
             (children (mapcar 'buffer-ribbon/list-of-windows-in-ribbon-order
                               children-wins)))
        (if is-vert-split
            (apply '-interleave children)
            (apply '-concat children))))))

(defun buffer-ribbon/buffer-ribbon-buffers-for-patch-grid (buffer-ribbon patch-grid)
  "Returns the buffers in the ribbon which should be 'in view' of the patch grid.

It's possible this is different from the actual buffers in the patch grid if
the buffers changed in the patch grid windows (and the buffer ribbon wasn't updated)
or the buffer ribbon was updated (and the patch grid wasn't)."
  (let* ((column (buffer-ribbon/patch-grid-column patch-grid))
         (start-offset (buffer-ribbon/column-to-offset buffer-ribbon column))
         (num-grid-tiles (buffer-ribbon/patch-grid-count patch-grid))
         (end-offset   (+ num-grid-tiles start-offset))
         (buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (-slice buffers start-offset end-offset)))

(defun buffer-ribbon/push-buffer-ribbon-to-patch-grid (buffer-ribbon patch-grid)
  (let* ((new-buffers (buffer-ribbon/buffer-ribbon-buffers-for-patch-grid
                       buffer-ribbon
                       patch-grid))
         (windows (buffer-ribbon/patch-grid-windows patch-grid))
         (window-buffer-pairs (-zip-pair windows new-buffers)))
    (mapcar
      (lambda (pair)
        (let ((window (car pair))
              (buffer (cdr pair)))
          (set-window-buffer window buffer)))
      window-buffer-pairs)))

(defun buffer-ribbon/current-buffers-from-windows ()
  (let ((wins (buffer-ribbon/list-of-windows-in-ribbon-order)))
    (mapcar 'window-buffer wins)))

(defun buffer-ribbon/patch-grid-buffers (patch-grid)
  (let ((windows (buffer-ribbon/patch-grid-windows patch-grid)))
    (mapcar 'window-buffer windows)))

(defun buffer-ribbon/update-buffer-ribbon-from-patch-grid (buffer-ribbon patch-grid)
  "replaces the part of the buffer ribbon which is visible on the patch
grid with the buffers in the patch grid"
  (let* ((buffers (buffer-ribbon/patch-grid-buffers patch-grid))
         (column (buffer-ribbon/patch-grid-column patch-grid))
         (offset (buffer-ribbon/column-to-offset buffer-ribbon column))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
         (new-buffers (buffer-ribbon/list-replace-at-offset old-buffers offset buffers)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon new-buffers)))

(defun buffer-ribbon/scroll-patch-grid-on-buffer-ribbon (buffer-ribbon patch-grid column-delta)
  (buffer-ribbon/update-buffer-ribbon-from-patch-grid buffer-ribbon patch-grid)
  (let* ((old-column (buffer-ribbon/patch-grid-column patch-grid))
         (ribbon-width (buffer-ribbon/buffer-ribbon-width buffer-ribbon))
         (new-column (+ old-column column-delta))
         (grid-width (buffer-ribbon/patch-grid-width patch-grid))
         (num-columns-to-prepend (- 0 new-column))
         (num-columns-to-append (- (+ new-column grid-width) ribbon-width)))
    (dotimes (_ num-columns-to-prepend)
      (buffer-ribbon/buffer-ribbon-prepend-column buffer-ribbon)
      (buffer-ribbon/set-patch-grid-column patch-grid (+ 1 old-column)))
    (dotimes (_ num-columns-to-append)
      (buffer-ribbon/buffer-ribbon-append-column buffer-ribbon)))
  (let* ((old-column (buffer-ribbon/patch-grid-column patch-grid)))
    (buffer-ribbon/set-patch-grid-column patch-grid (+ old-column column-delta)))
  (buffer-ribbon/push-buffer-ribbon-to-patch-grid buffer-ribbon patch-grid))

;;;; user-facing commands

(defun buffer-ribbon/init-from-current-windows ()
  (interactive)
  (let* ((buffer-ribbon (buffer-ribbon/make-buffer-ribbon))
         (current-windows (buffer-ribbon/list-of-windows-in-ribbon-order))
         (patch-grid (buffer-ribbon/make-patch-grid
                      buffer-ribbon
                      current-windows)))
    (setq buffer-ribbon/global-patch-grid patch-grid)
    (buffer-ribbon/update-buffer-ribbon-from-patch-grid buffer-ribbon patch-grid)))

(defun buffer-ribbon/shift-left ()
  (interactive)
  (buffer-ribbon/scroll-patch-grid-on-buffer-ribbon
   (buffer-ribbon/current-buffer-ribbon)
   (buffer-ribbon/current-patch-grid)
   +1))

(defun buffer-ribbon/shift-right ()
  (interactive)
  (buffer-ribbon/scroll-patch-grid-on-buffer-ribbon
   (buffer-ribbon/current-buffer-ribbon)
   (buffer-ribbon/current-patch-grid)
   -1))

(defun buffer-ribbon/zoom-selected-window ()
  (interactive)
  (when (buffer-ribbon/patch-grid-window-p (selected-window))
    (set-frame-parameter nil 'buffer-ribbon-window-config (current-window-configuration))
    (delete-other-windows)))

(defun buffer-ribbon/unzoom ()
  (interactive)
  (let ((window-config (frame-parameter nil 'buffer-ribbon-window-config)))
    (when window-config
      (set-window-configuration window-config)
      (set-frame-parameter nil 'buffer-ribbon-window-config nil))))

(defun buffer-ribbon/select-patch-grid-window (col row)
  "select the window in the patch grid at the given column and row.
0-based, i.e. row 0 is the top, row 1 is the row below that.
column 0 is the left, column 1 is the column to the right of that."
  (let* ((patch-grid (buffer-ribbon/current-patch-grid))
         (height (buffer-ribbon/patch-grid-height patch-grid))
         (windows (buffer-ribbon/patch-grid-windows patch-grid))
         (window-position (+ (* col height) row))
         (window (get-window-with-predicate
                  (lambda (window)
                    (= window-position
                       (buffer-ribbon/patch-grid-window-position window))))))
    (when window
      (select-window window))))

;; (add-hook
;;  'window-configuration-change-hook
;;  'buffer-ribbon/update-ribbon-buffers)

(provide 'buffer-ribbon)

