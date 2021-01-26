;;; cg-layout.el --- Personal window management -*- lexical-binding: t; -*-


(defun cg-layout-frame ()
  (interactive)
  (set-frame-height nil 105)
  (set-frame-width nil 538)
  (set-frame-position (selected-frame) 5 6))

(defun cg-layout ()
  (interactive)
  ;; On some screens the frame isn't big enough for splits despite
  ;; `cg-layout-frame'. Ignore the resulting errors and do what you can.
  (ignore-errors
    (cg-layout-frame)
    (delete-other-windows)
    (split-window-horizontally)
    (let ((center-split (split-window-horizontally)))
      (split-window-below 66)
      (split-window-vertically)
      (select-window center-split))))

(autoload 'windmove-find-other-window "windmove"
  "Return the window object in direction DIR.

\(fn dir &optional arg window)")

(declare-function windmove-find-other-window "windmove" (dir &optional arg window))

(defun cg-layout-window-in-frame (x y &optional frame)
  "Find Xth horizontal and Yth vertical window from top-left of FRAME."
  (let ((orig-x x) (orig-y y)
        (w (frame-first-window frame)))
    (while (and (windowp w) (> x 0))
      (setq w (windmove-find-other-window 'right 1 w)
            x (1- x)))
    (while (and (windowp w) (> y 0))
      (setq w (windmove-find-other-window 'down 1 w)
            y (1- y)))
    (unless (windowp w)
      (error "No window at (%d, %d)" orig-x orig-y))
    w))

(defun cg-layout-set-window-buffer-in-frame (x y buffer &optional frame)
  "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
  (set-window-buffer (cg-layout-window-in-frame x y frame) buffer))

(defun cg-layout-split-window-multiple-ways (x y)
  "Split the current frame into a grid of X columns and Y rows."
  (interactive "nColumns: \nnRows: ")
  ;; one window
  (delete-other-windows)
  (dotimes (i (1- x))
    (split-window-horizontally)
    (dotimes (j (1- y))
      (split-window-vertically))
    (other-window y))
  (dotimes (j (1- y))
    (split-window-vertically))
  (balance-windows))

(defun cg-layout-restore-or-default ()
  (interactive)
  (if (boundp 'saved-window-layout )
      (set-window-configuration saved-window-layout )
    (set-default-window-layout ) ) )

(defun cg-layout-save ()
  (interactive)
  (setq saved-window-layout (current-window-configuration ) ))

(global-set-key [f1] 'cg-layout-restore-or-default-window-layout)
(global-set-key [S-f1] 'cg-layout-save )

;; TODO: Check if this function actually works
(defun cg-layout-line-up ()
  (interactive)
  (when mark-active
    (put-text-property
     (min (point) (mark)) (max (mark) (point))
     'display `( space . ( :align-to 20 )))))

(cg-layout)

(provide 'cg-layout)
;;; cg-layout.el ends here
