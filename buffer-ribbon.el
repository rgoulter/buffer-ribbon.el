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

(setq buffer-ribbon-position 0)
(setq buffer-ribbon-buffers '())

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

(defun buffer-ribbon/init ()
  (interactive)
  (buffer-ribbon/init-ribbon-from-windows)
  (setq buffer-ribbon-position 0))

(defun buffer-ribbon/refresh ()
  (interactive)
  ;; assumes that buffer-ribbon-position
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

(defun buffer-ribbon/empty-buffer ()
  "function which returns a buffer to assign
as a 'default buffer' when moving the buffer-ribbon
past its defined"
  (get-buffer-create "*scratch*"))

(defun buffer-ribbon/current-buffers-from-windows ()
  (let ((wins (buffer-ribbon/list-of-windows-in-ribbon-order)))
    (mapcar 'window-buffer wins)))

(defun buffer-ribbon/init-ribbon-from-windows ()
    (setq buffer-ribbon-buffers (buffer-ribbon/current-buffers-from-windows)))


(setq buffer-ribbon-position 0)
(setq buffer-ribbon-buffers '())

(defun buffer-ribbon/update-ribbon-bufs (&rest _)
  (when-let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (let* ((old-ribbon buffer-ribbon-buffers)
             (pos buffer-ribbon-position)
             (new-head (-take pos old-ribbon))
             (new-tail (-drop (+ pos 6) old-ribbon))
             (current-bufs (buffer-ribbon/current-buffers-from-windows)))
        (setq buffer-ribbon-buffers (append new-head current-bufs new-tail))))))

;; (add-hook
;;  'window-configuration-change-hook
;;  'buffer-ribbon/update-ribbon-bufs)

(defun buffer-ribbon/adjust-ribbon-position (col-delta)
  "update buffer-ribbon-buffers by col-delta
so that buffer-ribbon-position and buffer-ribbon-buffers
can be applied in buffer-ribbon/refresh.

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
  (buffer-ribbon/refresh))

(defun buffer-ribbon/shift-left ()
  (interactive)
  (buffer-ribbon/adjust-ribbon-position +1))

(defun buffer-ribbon/shift-right ()
  (interactive)
  (buffer-ribbon/adjust-ribbon-position -1))

(provide 'buffer-ribbon)

