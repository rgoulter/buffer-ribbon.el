;;; buffer-ribbon.el --- a buffer ribbon  -*- lexical-binding:t -*-

;; Author: Richard Goulter <richard.goulter@gmail.com>
;; URL: https://github.com/rgoulter/buffer-ribbon.el
;; Keywords: convenience
;; Version: 0.1-git
;; Package-Requires: ((dash "2.13.0") (emacs "24.3"))

;;; Commentary:

;;; This file provides commands which are useful for
;;; easily setting up and manipulating a grid of windows,
;;; as well as "scrolling" a ribbon of buffers through this
;;; grid.

;;; Code:

(require 'cl)  ; for cddr, caddr, etc.
(require 'dash)

;;;; window grid methods

;; splitting vertically ('below) first, then horizontally
;; means that other-window navigates "right" across
;; the top, then "right" across the bottom.
(defun buffer-ribbon/split-into-3-2-v-h ()
  "Split the window into a 3x2 grid.

This is done by first splitting vertically, then horizontally.
This ordering means 'other-window' cycles across the top row
then the bottom row."
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
(defun buffer-ribbon/split-into-3-2 (&optional window)
  "Split the WINDOW into a 3x2 grid.

The selected window is used if WINDOW is nil.
This splits the grid so that 'other-window' cycles through
each column, left to right."
  (interactive)
  (let* ((window (or window (selected-window)))
         (mid-window (split-window window nil 'right))
         (right-window (split-window mid-window nil 'right))
         (windows (list window
                        (split-window window nil 'below)
                        mid-window
                        (split-window mid-window nil 'below)
                        right-window
                        (split-window right-window nil 'below))))
    (balance-windows)
    windows))

;;;; buffer ribbon methods

(defun buffer-ribbon/make-buffer-ribbon (&optional buffers column-height)
  "Construct a new buffer-ribbon.

BUFFERS (optional) is the initial list of buffers.
Defaults to a list of 6 'buffer-ribbon/empty-buffer'.
COLUMN-HEIGHT (option) defaults to 2."
  (list 'buffer-ribbon
        (or buffers
            (buffer-ribbon/empty-buffers 6))
        (or column-height 2)))

(defun buffer-ribbon/buffer-ribbon-p (o)
  "Predicate for whether O is a buffer-ribbon."
  (and (listp o)
       (eq 'buffer-ribbon (car o))
       (listp (cadr o))
       (integerp (caddr o))))

(defun buffer-ribbon/buffer-ribbon-buffers (ribbon)
  "Get the buffers of the buffer-ribbon RIBBON."
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (cadr ribbon)
    (signal 'wrong-type-argument (list #'buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/set-buffer-ribbon-buffers (ribbon new-buffers)
  "Set the buffer-ribbon RIBBON to have buffers NEW-BUFFERS."
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (setcdr ribbon
              (cons new-buffers
                    (list (caddr ribbon))))
    (signal 'wrong-type-argument (list #'buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/buffer-ribbon-height (ribbon)
  "The column height of the buffer-ribbon RIBBON."
  (if (buffer-ribbon/buffer-ribbon-p ribbon)
      (caddr ribbon)
    (signal 'wrong-type-argument (list #'buffer-ribbon/buffer-ribbon-p ribbon))))

(defun buffer-ribbon/current-buffer-ribbon ()
  "The 'current' buffer-ribbon.

This is a convenience method for looking up the buffer-ribbon
associated with the current patch-grid which owns
the current buffer-ribbon."
  (let ((patch-grid (buffer-ribbon/current-patch-grid)))
    (when patch-grid
      (buffer-ribbon/patch-grid-buffer-ribbon patch-grid))))

(defun buffer-ribbon/empty-buffer ()
  "Return a buffer to assign as a blank/default buffer.

Empty buffer is used when the buffer-ribbon is
scrolled past its content, or when a grid tile
in the patch grid is cleared."
  (let ((empty (get-buffer-create "*empty*")))
    (with-current-buffer empty (set-buffer-modified-p nil))
    empty))

(defun buffer-ribbon/empty-buffers (n)
  "Return a list of N buffers created by 'buffer-ribbon/empty-buffer'."
  (mapcar (lambda (_) (buffer-ribbon/empty-buffer))
          (number-sequence 1 n)))

(defun buffer-ribbon/list-replace-at-offset (old-list offset new-content)
  "Return a list from OLD-LIST with content replaced at OFFSET by NEW-CONTENT."
  (let* ((len (length new-content))
         (new-head (-take offset old-list))
         (new-tail (-drop (+ offset len) old-list)))
    (append new-head
            new-content
            new-tail)))

(defun buffer-ribbon/buffer-ribbon-width (buffer-ribbon)
  "Get the column-width of the BUFFER-RIBBON.

For example, if a BUFFER-RIBBON contains a list of 30 consecutive
buffers and has a column height of 2, then it is 15 columns wide."
  (let ((buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
        (column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (/ (length buffers) column-height)))

(defun buffer-ribbon/column-to-offset (buffer-ribbon column)
  "Get the offset in the list of BUFFER-RIBBON buffers for a given COLUMN.

For example, if a BUFFER-RIBBON has a column height of 2, then
the offset in the list of buffers for column 10 is 20."
  (let ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (* column column-height)))

(defun buffer-ribbon/offset-to-column (buffer-ribbon offset)
  "Get the column of the BUFFER-RIBBON for the given OFFSET.

For example, if a BUFFER-RIBBON has a column height of 2, then
the offset of 20 in the list of buffers is 10."
  (let ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon)))
    (/ offset column-height)))

(defun buffer-ribbon/buffer-ribbon-append-column (buffer-ribbon)
  "Append a column of empty-buffers to the BUFFER-RIBBON."
  (let* ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (add-buffers (buffer-ribbon/empty-buffers column-height))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon (append old-buffers add-buffers))))

(defun buffer-ribbon/buffer-ribbon-prepend-column (buffer-ribbon)
  "Prepend a column of empty-buffers to the BUFFER-RIBBON.

The prepended column becomes the 0th column.
Note that any patch grids viewing this BUFFER-RIBBON need to update
their position accordingly."
  (let* ((column-height (buffer-ribbon/buffer-ribbon-height buffer-ribbon))
         (add-buffers (buffer-ribbon/empty-buffers column-height))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon (append add-buffers old-buffers))))

;;;; patch grid methods

(defvar buffer-ribbon/global-patch-grid-hash (make-hash-table)
  "The hash table with the hash table of patch grids, keyed by name.

Use 'buffer-ribbon/current-patch-grid' function instead of accessing this directly.")

(defun buffer-ribbon/set-patch-grid-window-parameters (window patch-grid-position)
  "Set the window parameters of WINDOW, i.e. PATCH-GRID-POSITION."
  (set-window-parameter window 'is-patch-grid t)
  (set-window-parameter window 'patch-grid-position patch-grid-position))

(defun buffer-ribbon/patch-grid-window-p (window)
  "Whether the WINDOW is a part of a patch-grid."
  (window-parameter window 'is-patch-grid))

(defun buffer-ribbon/patch-grid-window-position (window)
  "The position of the patch-grid that the WINDOW is associated with."
  (window-parameter window 'patch-grid-position))

(defun buffer-ribbon/window-patch-grid-name (window)
  "The name of the patch-grid that the WINDOW is associated with."
  (window-parameter window 'patch-grid-name))

(defun buffer-ribbon/set-window-patch-grid-name (window name)
  "Set the NAME of the patch-grid that the WINDOW is associated with."
  (set-window-parameter window 'patch-grid-name name))

(defun buffer-ribbon/make-patch-grid (buffer-ribbon windows)
  "Construct a new patch-grid with the given BUFFER-RIBBON and WINDOWS.

WINDOWS are updated with 'buffer-ribbon/set-patch-grid-window-parameters'."
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
  "Whether the given O is a patch-grid."
  (and (listp o)
       (eq 'patch-grid (car o))
       (buffer-ribbon/buffer-ribbon-p (cadr o))
       (integerp (caddr o))
       (listp (cadddr o))))

(defun buffer-ribbon/patch-grid-buffer-ribbon (patch-grid)
  "Get the buffer-ribbon of PATCH-GRID to NEW-BUFFER-RIBBON."
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (cadr patch-grid)
    (signal 'wrong-type-argument (list #'buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-buffer-ribbon (patch-grid new-buffer-ribbon)
  "Set the buffer-ribbon of PATCH-GRID to NEW-BUFFER-RIBBON.

Note: the windows of the PATCH-GRID are not updated."
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons new-buffer-ribbon
                    (list (caddr patch-grid)
                          (cadddr patch-grid))))
    (signal 'wrong-type-argument (list #'buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/patch-grid-column (patch-grid)
  "Get the column of PATCH-GRID.

This is the scroll-position against the buffer-ribbon."
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (caddr patch-grid)
    (signal 'wrong-type-argument (list #'buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-grid-column (patch-grid new-column)
  "Set the column of PATCH-GRID to NEW-COLUMN.

This is the scroll-position against the buffer-ribbon."
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons (cadr patch-grid)
                    (list new-column
                          (cadddr patch-grid))))
    (signal 'wrong-type-argument (list #'buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/patch-grid-windows (patch-grid)
  "Get the windows of the PATCH-GRID."
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (cadddr patch-grid)
    (signal 'wrong-type-argument (list #'buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/set-patch-grid-windows (patch-grid new-windows)
  "Set the windows of the PATCH-GRID to NEW-WINDOWS.

Note: the windows are not updated."
  (if (buffer-ribbon/patch-grid-p patch-grid)
      (setcdr patch-grid
              (cons (cadr patch-grid)
                    (caddr patch-grid)
                    (list new-windows)))
    (signal 'wrong-type-argument (list #'buffer-ribbon/patch-grid-p patch-grid))))

(defun buffer-ribbon/patch-grid (name)
  "Get the patch-grid with the given NAME.

It is assumed that the patch-grid was registered using
'buffer-ribbon/register-patch-grid', which assigns
the NAME to the windows of the patch-grid."
  (gethash name buffer-ribbon/global-patch-grid-hash))

(defun buffer-ribbon/window-patch-grid (window)
  "Get the patch-grid associated with WINDOW.

This uses the name of the patch grid stored in the window parameters
of WINDOW.
It is assumed that the patch-grid was registered using
'buffer-ribbon/register-patch-grid'."
  (let ((patch-grid-name (buffer-ribbon/window-patch-grid-name window)))
    (buffer-ribbon/patch-grid patch-grid-name)))

(defmacro buffer-ribbon/check-window-is-patch-grid-tile (window &rest body)
  "Check whether the WINDOW is a patch grid tile.

BODY is called if it is, and throws warning if not."
  `(let ((window ,window))
     (if (buffer-ribbon/patch-grid-window-p window)
         (if (buffer-ribbon/window-patch-grid-name window)
             (progn ,@body)
           (display-warning 'buffer-ribbon "no patch grid registered with window"))
       (display-warning 'buffer-ribbon "window is not a patch grid tile"))))

(defun buffer-ribbon/current-patch-grid ()
  "Get the current patch grid associated with the selected window.

This is a convenience wrapper around 'buffer-ribbon/window-patch-grid'."
  (buffer-ribbon/window-patch-grid (selected-window)))

(defun buffer-ribbon/next-unique-key (hash key &optional number)
  "Find a next key which will be unique for the HASH table.

KEY must be a string, and is used as the key prefix.
NUMBER is appended to the KEY.

For example, if KEY is 'my-key, then the results inserted into a hash
table would be 'my-key, 'my-key<2>, 'my-key<3>, etc."
  (let ((key-candidate (intern (if number (format "%s<%d>" key number) key))))
    (if (not (gethash key-candidate hash))
        key-candidate
      (buffer-ribbon/next-unique-key
       hash
       key
       (if number (+ 1 number) 2)))))

(defun buffer-ribbon/next-patch-grid-name ()
  "The next unique key-name for the global patch grid hash table.

Typically, 'patch-grid, 'patch-grid<2>, 'patch-grid<3>, etc."
  (buffer-ribbon/next-unique-key
   buffer-ribbon/global-patch-grid-hash
   "patch-grid"))

(defun buffer-ribbon/register-patch-grid (patch-grid)
  "Update 'buffer-ribbon/global-patch-grid-hash' with the PATCH-GRID.

Also update the window parameters of the windows in the PATCH-GRID
with the name generated for the patch grid."
  (let ((name (buffer-ribbon/next-patch-grid-name))
        (windows (buffer-ribbon/patch-grid-windows patch-grid)))
    (puthash name patch-grid buffer-ribbon/global-patch-grid-hash)
    (mapcar (lambda (window)
              (buffer-ribbon/set-window-patch-grid-name window name))
            windows)))

(defun buffer-ribbon/patch-grid-width (&optional patch-grid)
  "Number of columns the PATCH-GRID has."
  3)

(defun buffer-ribbon/patch-grid-height (&optional patch-grid)
  "Number of rows the PATCH-GRID has."
  2)

(defun buffer-ribbon/patch-grid-count (&optional patch-grid)
  "Number of tiles the PATCH-GRID has."
  (* (buffer-ribbon/patch-grid-width)
     (buffer-ribbon/patch-grid-height)))

(defun buffer-ribbon/patch-grid-window-live-ps (patch-grid)
  "Return list of whether the patch grid windows are live.

e.g. If all of the windows in a 3x2 patch grid are live, the result
will be '(t t t t t t). If only the middle-top on is live, then the
result will be '(nil nil t nil nil nil)."
  (let ((windows (buffer-ribbon/patch-grid-windows patch-grid)))
    (mapcar #'window-live-p windows)))

;; this is useful because (window-list) returns
;; in an order I might not like
;;
;; this function returns in an order of columns I like
;; for windows which are horizontal splits of vert splits
;; (or vert splits of horizontal splits)
(defun buffer-ribbon/list-of-windows-in-ribbon-order (&optional win)
  "Get the list of windows in WIN in a top-down, left-to-right order."
  (let ((win (or win (car (window-tree)))))
    (if (windowp win)
        (list win)
      (let* ((is-vert-split (car win))
             (children-wins (cddr win))
             (children (mapcar #'buffer-ribbon/list-of-windows-in-ribbon-order
                               children-wins)))
        (if is-vert-split
            (apply #'-interleave children)
          (apply #'-concat children))))))

(defun buffer-ribbon/buffer-ribbon-buffers-for-patch-grid (buffer-ribbon patch-grid)
  "Get the buffers in the BUFFER-RIBBON that should be in the PATCH-GRID view.

It's possible this is different from the actual buffers in the patch
grid if the buffers changed in the patch grid windows (and the buffer
ribbon wasn't updated) or the buffer ribbon was updated (and the patch
grid wasn't)."
  (let* ((column (buffer-ribbon/patch-grid-column patch-grid))
         (start-offset (buffer-ribbon/column-to-offset buffer-ribbon column))
         (num-grid-tiles (buffer-ribbon/patch-grid-count patch-grid))
         (end-offset   (+ num-grid-tiles start-offset))
         (buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon)))
    (-slice buffers start-offset end-offset)))

(defun buffer-ribbon/push-buffer-ribbon-to-patch-grid (buffer-ribbon patch-grid)
  "Set the buffers in windows of PATCH-GRID from the view of BUFFER-RIBBON.

The buffers from BUFFER-RIBBON are taken acording to
'buffer-ribbon/buffers-for-patch-grid'."
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
  "Get buffers from the windows in the current frame.

The buffers are sorted in the same order as
'buffer-ribbon/list-of-windows-in-ribbon-order'."
  (let ((wins (buffer-ribbon/list-of-windows-in-ribbon-order)))
    (mapcar #'window-buffer wins)))

(defun buffer-ribbon/patch-grid-buffers (patch-grid)
  "Get the buffers of the windows of the PATCH-GRID."
  (let ((windows (buffer-ribbon/patch-grid-windows patch-grid)))
    (mapcar #'window-buffer windows)))

(defun buffer-ribbon/update-buffer-ribbon-from-patch-grid (buffer-ribbon patch-grid)
  "Update the BUFFER-RIBBON with the current buffers in PATCH-GRID.

Replace the part of the buffer-ribbon which is visible on
the patch-grid with the buffers in the patch-grid."
  (let* ((buffers (buffer-ribbon/patch-grid-buffers patch-grid))
         (column (buffer-ribbon/patch-grid-column patch-grid))
         (offset (buffer-ribbon/column-to-offset buffer-ribbon column))
         (old-buffers (buffer-ribbon/buffer-ribbon-buffers buffer-ribbon))
         (new-buffers (buffer-ribbon/list-replace-at-offset old-buffers offset buffers)))
    (buffer-ribbon/set-buffer-ribbon-buffers buffer-ribbon new-buffers)))

(defun buffer-ribbon/scroll-patch-grid-on-buffer-ribbon (buffer-ribbon patch-grid column-delta)
  "Scroll the BUFFER-RIBBON in PATCH-GRID by COLUMN-DELTA.

A positive COLUMN-DELTA moves the PATCH-GRID to the right by
that number of columns.
A negative COLUMN-DELTA moves the PATCH-GRID to the left by
that number of columns."
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

;;;###autoload
(defun buffer-ribbon/init-patch-grid-using-selected-window (&optional window)
  "Construct a patch-grid using the selected WINDOW.

WINDOW defaults to 'selected-window' if nil."
  (interactive)
  (let* ((window (or window (selected-window)))
         (buffer-ribbon (buffer-ribbon/make-buffer-ribbon))
         (grid-windows (buffer-ribbon/split-into-3-2 window))
         (patch-grid (buffer-ribbon/make-patch-grid
                      buffer-ribbon
                      grid-windows)))
    (buffer-ribbon/register-patch-grid patch-grid)
    (buffer-ribbon/update-buffer-ribbon-from-patch-grid buffer-ribbon patch-grid)))

;;;###autoload
(defun buffer-ribbon/init-patch-grid-using-new-frame ()
  "Make a new frame and construct a patch-grid using that."
  (interactive)
  (let ((new-frame (make-frame)))
    (select-frame new-frame)
    (buffer-ribbon/init-patch-grid-using-selected-window)))

;;;###autoload
(defun buffer-ribbon/init-patch-grid-using-current-windows ()
  "Use this command if you already have a 3x2 grid.

This grid of window tiles becomes a patch-grid.

Note that all windows (excluding minibuffer) in the
frame will be considered for the patch-grid."
  (interactive)
  (let* ((buffer-ribbon (buffer-ribbon/make-buffer-ribbon))
         (current-windows (buffer-ribbon/list-of-windows-in-ribbon-order))
         (patch-grid (buffer-ribbon/make-patch-grid
                      buffer-ribbon
                      current-windows)))
    (buffer-ribbon/register-patch-grid patch-grid)
    (buffer-ribbon/update-buffer-ribbon-from-patch-grid buffer-ribbon patch-grid)))

;;;###autoload
(defun buffer-ribbon/scroll-buffer-ribbon-left ()
  "Move the ribbon left along the patch grid.

Buffers which were to the right of the patch grid view
will come into view."
  (interactive)
  (buffer-ribbon/check-window-is-patch-grid-tile (selected-window)
    (buffer-ribbon/scroll-patch-grid-on-buffer-ribbon
     (buffer-ribbon/current-buffer-ribbon)
     (buffer-ribbon/current-patch-grid)
     +1)))

;;;###autoload
(defun buffer-ribbon/scroll-patch-grid-right ()
  "Move the patch grid right along the ribbon.

Buffers which were to the right of the patch grid view
will come into view."
  (interactive)
  (buffer-ribbon/check-window-is-patch-grid-tile (selected-window)
    (buffer-ribbon/scroll-patch-grid-on-buffer-ribbon
     (buffer-ribbon/current-buffer-ribbon)
     (buffer-ribbon/current-patch-grid)
     +1)))

;;;###autoload
(defun buffer-ribbon/scroll-buffer-ribbon-right ()
  "Move the buffer ribbon right along the patch grid.

Buffers which were to the left of the patch grid view
will come into view."
  (interactive)
  (buffer-ribbon/check-window-is-patch-grid-tile (selected-window)
    (buffer-ribbon/scroll-patch-grid-on-buffer-ribbon
     (buffer-ribbon/current-buffer-ribbon)
     (buffer-ribbon/current-patch-grid)
     -1)))

;;;###autoload
(defun buffer-ribbon/scroll-patch-grid-left ()
  "Move the patch grid left along the buffer ribbon.

Buffers which were to the left of the patch grid view
will come into view."
  (interactive)
  (buffer-ribbon/check-window-is-patch-grid-tile (selected-window)
    (buffer-ribbon/scroll-patch-grid-on-buffer-ribbon
     (buffer-ribbon/current-buffer-ribbon)
     (buffer-ribbon/current-patch-grid)
     -1)))

;;;###autoload
(defun buffer-ribbon/zoom-selected-window ()
  "'Zoom' into the selected window.

This makes the selected window the only window in the frame.
The window configuration is saved in a window parameter.

To zoom back out to the window configuration when this command
was called, call 'buffer-ribbon/unzoom'."
  (interactive)
  (buffer-ribbon/check-window-is-patch-grid-tile (selected-window)
    (set-frame-parameter nil #'buffer-ribbon-window-config (current-window-configuration))
    (delete-other-windows)))

;;;###autoload
(defun buffer-ribbon/unzoom ()
  "'Zoom' out back to the patch-grid view.

Restore the window configuration to what it was when
'buffer-ribbon/zoom-selected-window' was called."
  (interactive)
  (buffer-ribbon/check-window-is-patch-grid-tile (selected-window)
    (let ((window-config (frame-parameter nil 'buffer-ribbon-window-config)))
      (when window-config
        (set-window-configuration window-config)
        (set-frame-parameter nil 'buffer-ribbon-window-config nil)))))

;;;###autoload
(defun buffer-ribbon/select-patch-grid-window (col row &optional window)
  "Select the WINDOW in the patch-grid at the given COL and ROW.

0-based, i.e. row 0 is the top, row 1 is the row below that.
column 0 is the left, column 1 is the column to the right of that."
  (let ((window (or window (selected-window))))
    (buffer-ribbon/check-window-is-patch-grid-tile window
      (let* ((patch-grid (buffer-ribbon/current-patch-grid))
             (height (buffer-ribbon/patch-grid-height patch-grid))
             (windows (buffer-ribbon/patch-grid-windows patch-grid))
             (window-position (+ (* col height) row))
             (target-window
              (get-window-with-predicate
               (lambda (window)
                 (= window-position
                    (buffer-ribbon/patch-grid-window-position window))))))
        (when target-window
          (select-window target-window))))))

;; (add-hook
;;  'window-configuration-change-hook
;;  'buffer-ribbon/update-ribbon-buffers)

(provide 'buffer-ribbon)

;;; buffer-ribbon.el ends here
