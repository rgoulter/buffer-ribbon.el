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

(defvar buffer-ribbon/global-ribbon nil
  "use buffer-ribbon/current-ribbon function instead of accessing this directly")

(defun buffer-ribbon/make-buffer-ribbon (&optional buffers column-height)
  (list 'buffer-ribbon
        0
        (or buffers
            (buffer-ribbon/empty-buffers 6))
        (or column-height 2)))

(defun buffer-ribbon/buffer-ribbon-p (o)
  (and (listp o)
       (eq 'buffer-ribbon (car o))
       (integerp (cadr o))
       (listp (caddr o))
       (integerp (cadddr o))))

(defun buffer-ribbon/buffer-ribbon-position (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (cadr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/set-buffer-ribbon-position (ribbon new-position)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (setcdr ribbon
              (cons new-position
                    (list (caddr ribbon)
                          (cadddr ribbon))))
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/buffer-ribbon-buffers (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (caddr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/set-buffer-ribbon-buffers (ribbon new-buffers)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (setcdr ribbon
              (cons (cadr ribbon)
                    (list new-buffers
                          (cadddr ribbon))))
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/buffer-ribbon-height (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (cadddr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/current-buffer-ribbon ()
  buffer-ribbon/global-ribbon)

(defun buffer-ribbon/init ()
  (interactive)
  (buffer-ribbon/init-ribbon-from-windows)
  (setq buffer-ribbon-position 0))

(defun buffer-ribbon/init-ribbon-from-windows ()
  (setq buffer-ribbon-buffers
        (buffer-ribbon/current-buffers-from-windows)))

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

(defun buffer-ribbon/buffer-ribbon-set-buffers-at-position (buffer-ribbon new-buffers)
  "replaces the buffers at the ribbon's position with this"
  (let* ((pos (buffer-ribbon/buffer-ribbon-position buffer-ribbon))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
         (new-buffers (buffer-ribbon/list-replace-at-offset old-buffers pos new-buffers)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon new-buffers)))

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

(defun buffer-ribbon/make-patch-grid (root-window windows)
  (let ((patch-grid (list 'patch-grid
                          root-window
                          windows)))
    ;; assign parameters to each of the window
    (-each-indexed windows
                   (lambda (index window)
                     (buffer-ribbon/set-patch-grid-window-parameters window index)))
    patch-grid))

(defun buffer-ribbon/patch-grid-p (o)
  (and (listp o)
       (eq 'patch-grid (car o))
       ;; (windowp (cadr o))
       (listp (caddr o))))

(defun buffer-ribbon/patch-grid-root-window (patch-grid)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (cadr patch-grid)
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-grid-root-window (patch-grid new-root-window)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons new-root-window
                    (list (caddr patch-grid))))
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/patch-grid-windows (patch-grid)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (caddr patch-grid)
      (signal 'wrong-type-argument (list buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-grid-windows (patch-grid new-windows)
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons (cadr patch grid)
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
  "returns the buffers in the ribbon which are 'in view' of the patch grid"
  (let* ((start (buffer-ribbon/buffer-ribbon-position buffer-ribbon))
         (num-grid-tiles (buffer-ribbon/patch-grid-count patch-grid))
         (end   (+ num-grid-tiles start))
         (buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (-slice buffers start end)))

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

(defun buffer-ribbon/update-buffer-ribbon-from-patch-grid (buffer-ribbon patch-grid)
  "replaces the part of the buffer ribbon which is visible on the patch
grid with the buffers in the patch grid"
  (let* ((windows (buffer-ribbon/patch-grid-windows patch-grid))
         (buffers (mapcar 'window-buffer windows)))
    (buffer-ribbon/buffer-ribbon-set-buffers-at-position buffer-ribbon buffers)))

(defun buffer-ribbon/buffer-ribbon-buffer-count (buffer-ribbon patch-grid)
  (let ((buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (length buffers)))

(defun buffer-ribbon/buffer-ribbon-width (buffer-ribbon)
  (let ((buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
        (column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (/ (length buffers) column-height)))

(defun buffer-ribbon/buffer-ribbon-column (buffer-ribbon)
  (let ((position (buffer-ribbon/buffer-ribbon-position buffer-ribbon))
        (column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (/ position column-height)))

(defun buffer-ribbon/buffer-ribbon-append-column (buffer-ribbon)
  (let* ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (add-buffers (buffer-ribbon/empty-buffers column-height))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon (append old-buffers add-buffers))))

(defun buffer-ribbon/buffer-ribbon-prepend-column (buffer-ribbon)
  (let* ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (add-buffers (buffer-ribbon/empty-buffers column-height))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
         (old-position (buffer-ribbon/buffer-ribbon-position buffer-ribbon)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon (append add-buffers old-buffers))
    (buffer-ribbon/set-buffer-ribbon-position buffer-ribbon (+ old-position column-height))))

(defun buffer-ribbon/adjust-ribbon-position (buffer-ribbon patch-grid column-delta)
  (buffer-ribbon/update-buffer-ribbon-from-patch-grid buffer-ribbon patch-grid)
  (let* ((ribbon-column (buffer-ribbon/buffer-ribbon-column buffer-ribbon))
         (ribbon-width (buffer-ribbon/buffer-ribbon-width buffer-ribbon))
         (new-column (+ ribbon-column column-delta))
         (grid-width (buffer-ribbon/patch-grid-width patch-grid))
         (num-columns-to-prepend (- 0 new-column))
         (num-columns-to-append (- (+ new-column grid-width) ribbon-width)))
    (dotimes (_ num-columns-to-prepend)
      (buffer-ribbon/buffer-ribbon-prepend-column buffer-ribbon))
    (dotimes (_ num-columns-to-append)
      (buffer-ribbon/buffer-ribbon-append-column buffer-ribbon)))
  (let* ((old-position (buffer-ribbon/buffer-ribbon-position buffer-ribbon))
         (column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (delta-position (* column-height column-delta)))
    (buffer-ribbon/set-buffer-ribbon-position buffer-ribbon (+ old-position delta-position)))
  (buffer-ribbon/push-buffer-ribbon-to-patch-grid buffer-ribbon patch-grid))

;;;; user-facing commands

(defun buffer-ribbon/init-from-current-windows ()
  (interactive)
  (let* ((root-win nil)  ;; I think this should be the "window-tree" root
         (current-windows (buffer-ribbon/list-of-windows-in-ribbon-order))
         (patch-grid (buffer-ribbon/make-patch-grid root-win current-windows))
         (buffer-ribbon (buffer-ribbon/make-buffer-ribbon)))
    (setq buffer-ribbon/global-ribbon buffer-ribbon)
    (setq buffer-ribbon/global-patch-grid patch-grid)
    (buffer-ribbon/update-buffer-ribbon-from-patch-grid buffer-ribbon patch-grid)))

(defun buffer-ribbon/shift-left ()
  (interactive)
  (buffer-ribbon/adjust-ribbon-position (buffer-ribbon/current-buffer-ribbon)
                                        (buffer-ribbon/current-patch-grid)
                                        +1))

(defun buffer-ribbon/shift-right ()
  (interactive)
  (buffer-ribbon/adjust-ribbon-position (buffer-ribbon/current-buffer-ribbon)
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

;; (add-hook
;;  'window-configuration-change-hook
;;  'buffer-ribbon/update-ribbon-buffers)

(provide 'buffer-ribbon)

