;;;;
;; window grid methods
;;;;

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

;;;;
;; "dummy" buffer methods
;;;;

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

;;;;
;; buffer ribbon methods
;;;;

(setq buffer-ribbon-position 0)
(setq buffer-ribbon-buffers '())

(defvar buffer-ribbon/global-ribbon nil
  "use buffer-ribbon/current-ribbon function instead of accessing this directly")

(defun buffer-ribbon/make-buffer-ribbon (&optional buffers)
  (list 'buffer-ribbon
        0
        (or buffers
            (mapcar (lambda (_) (buffer-ribbon/empty-buffer))
                    (number-sequence 1 6)))))

(defun buffer-ribbon/buffer-ribbon-p (o)
  (and (listp o)
       (eq 'buffer-ribbon (car o))
       (integerp (cadr o))
       (listp (caddr o))))

(defun buffer-ribbon/buffer-ribbon-position (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (cadr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/set-buffer-ribbon-position (ribbon new-position)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (setcdr ribbon
              (cons new-position
                    (list (caddr ribbon))))
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/buffer-ribbon-buffers (ribbon)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (caddr ribbon)
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/set-buffer-ribbon-buffers (ribbon new-buffers)
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (setcdr ribbon
              (cons (cadr ribbon)
                    (list new-buffers)))
      (signal 'wrong-type-argument (list buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/current-ribbon ()
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

;;;;
;; patch grid methods
;;;;

(defvar buffer-ribbon/global-patch-grid nil
  "use buffer-ribbon/current-patch-grid function instead of accessing this directly")

(defun buffer-ribbon/set-patch-grid-window-parameters (window patch-grid-position)
  (set-window-parameter window 'is-patch-grid t)
  (set-window-parameter window 'patch-grid-position patch-grid-position))

(defun buffer-ribbon/patch-grid-window-p (window)
  (window-parameter window 'is-patched-grid))

(defun buffer-ribbon/patch-grid-window-position (window)
  (window-parameter window 'patch-grid-position))

(defun buffer-ribbon/make-patch-grid (root-window windows)
  (let ((patch-grid (list 'patch-grid
                          root-window
                          windows)))
    ;; assign parameters to each of the window
    (-each-indexed (lambda (index window)
                     (buffer-ribbon/set-patch-grid-window-parameters window index))
                   windows)
    patch-grid))

(defun buffer-ribbon/patch-grid-p (o)
  (and (listp o)
       (eq 'patch-grid (car o))
       (windowp (cadr o))
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

(defun buffer-ribbon/push-buffer-ribbon-to-patch-grid ()
  (let* ((start buffer-ribbon-position)
         (end   (+ 6 buffer-ribbon-position))
         (new-buffers (-slice buffer-ribbon-buffers start end))
         (wins (buffer-ribbon/list-of-windows-in-ribbon-order))
         (win-buf-pairs (-zip-pair wins new-buffers)))
    (mapcar
      (lambda (pair)
        (let ((win (car pair))
              (buf (cdr pair)))
          (set-window-buffer win buf)))
      win-buf-pairs)))

(defun buffer-ribbon/current-buffers-from-windows ()
  (let ((wins (buffer-ribbon/list-of-windows-in-ribbon-order)))
    (mapcar 'window-buffer wins)))

(defun buffer-ribbon/update-ribbon-buffers (&rest _)
  "replaces the part of the buffer ribbon which is visible on the patch
grid with the buffers in the patch grid"
  (when-let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (let* ((old-ribbon buffer-ribbon-buffers)
             (pos buffer-ribbon-position)
             (new-head (-take pos old-ribbon))
             (new-tail (-drop (+ pos 6) old-ribbon))
             (current-buffers (buffer-ribbon/current-buffers-from-windows)))
        (setq buffer-ribbon-buffers
              (append new-head
                      current-buffers
                      new-tail))))))

;;; this method is pretty brutal.
;;; can simplify it as: ribbon-num-cols;
;;; and operating in terms of columns.
;;; that ought to simplify this enough.
(defun buffer-ribbon/adjust-ribbon-position (col-delta)
  "update buffer-ribbon-buffers by col-delta
so that buffer-ribbon-position and buffer-ribbon-buffers
can be applied in buffer-ribbon/push-buffer-ribbon-to-patch-grid.

At the moment this is pretty kludge-y,
since it's just a proof of concept"
  (setq buffer-ribbon-position
        (+ buffer-ribbon-position (* 2 col-delta)))
  ;; buffer-ribbon-pos < 0 means that
  ;; buffers need to be prepended
  (while (< buffer-ribbon-position 0)
    (dotimes (i 2)  ;; 2x, because 2x rows in ribbon
      (let ((buf (buffer-ribbon/empty-buffer)))
        (setq buffer-ribbon-buffers (cons buf buffer-ribbon-buffers))))
    (setq buffer-ribbon-position
          (+ buffer-ribbon-position 2)))
  ;; check if position + 6 > length of the list
  ;; ...this feels inefficient; but for proof of concept
  (while (>= (+ 6 buffer-ribbon-position)
             (length buffer-ribbon-buffers))
    (dotimes (i 2)  ;; 2x, because 2x rows in ribbon
      (let ((buf (buffer-ribbon/empty-buffer)))
        (setq buffer-ribbon-buffers
              (append buffer-ribbon-buffers
                      (list buf))))))
  (buffer-ribbon/push-buffer-ribbon-to-patch-grid))

;;;;
;; user-facing commands
;;;;

(defun buffer-ribbon/shift-left ()
  (interactive)
  (buffer-ribbon/adjust-ribbon-position +1))

(defun buffer-ribbon/shift-right ()
  (interactive)
  (buffer-ribbon/adjust-ribbon-position -1))

;; (add-hook
;;  'window-configuration-change-hook
;;  'buffer-ribbon/update-ribbon-buffers)

(provide 'buffer-ribbon)

