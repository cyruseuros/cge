;;; cg-layout.el --- Personal window management -*- lexical-binding: t; -*-

;; *cg* position initial window */
(defun cg-layout ()
  "set window pos and splits to default"
  (interactive)
  (delete-other-windows)
  (ignore-errors
    (cg-layout-frame))
  ;; Prevent things that aren't user commands from splitting our windows
  (set-frame-parameter nil 'unsplittable t)
  ;; `setq' instead of `let'. We'll use them later
  (setq my-curbuf (current-buffer)
        my-right-window (split-window-right)
        my-middle-window (split-window-right)
        my-big-parrent-window (window-parent my-middle-window))
  (balance-windows my-big-parrent-window)
  (setq my-bottom-left-window (condition-case nil
                                  (split-window-below 66)
                                ;; Handle excessively small screens like mine.
                                (error (split-window-below)))
        my-middle-left-window (split-window-below)
        my-top-left-window (frame-first-window)
        my-little-parent-window (window-parent my-top-left-window))
  (select-window my-middle-window))

(defvar cg-layout-windows
  '(my-right-window
    my-middle-window
    my-bottom-left-window
    my-middle-left-window
    my-top-left-window))

(defun cg-layout-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

;; Could be a macro but it's likely overkill.
;; Elisp is a lisp-2 so we can reuse the names
(dolist (win cg-layout-windows)
  (fset win (lambda (buffer alist)
              (when (not (member buffer (cg-layout-visible-buffers)))
                ;; Only do anything on buffers that are not already visible.
                ;; Don't focus them either let the default code take it from here.
                (set-window-buffer (symbol-value win) buffer)))))

;; This is where you define window behaviour It's more complicated than that,
;; but for your use case, just make each member of the alist of the form
;; (REGEX/MATCHER-FUNCTION (FUNCTION [. ACTIONS])). The docs go into more detail
;; but I don't think you'll need it
(setq display-buffer-overriding-action nil
      display-buffer-alist
      '(("\\*shell\\*" (my-bottom-left-window))
        ("\\*Completions\\*" (my-middle-left-window))
        ("\\*compilation\\*" (my-bottom-left-window)))
      display-buffer-base-action '(my-top-left-window))

;; Your current window configuration breaks `ediff'. I can write a custom
;; `ediff-window-setup-function' but it just makes certain assumptions about
;; windows sizes and their relationship that simply aren't true in your case.
;; Changing the window configuration kind of defeates the purpose of not
;; disrupting your workflow. For now, I'll just make sure that `ediff' drops you
;; back right where you were before.
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-right)

(defun cg-layout-frame ()
  (interactive)
  (set-frame-height nil 105)
  (set-frame-width nil 538)
  (set-frame-position (selected-frame) 5 6))


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

(defvar cg-layout-snapshots '())

(defun cg-layout-push-snapshot ()
  "Store current window configuration in the `cg-layout-snapshots' stack."
  (interactive)
  ;; let messy function do what it wants
  (set-frame-parameter nil 'unsplittable nil)
  (push (current-window-configuration) cg-layout-snapshots))

(defun cg-layout-pop-snapshot ()
  "Restore most recent window snapshot from the `cg-layout-snapshots' stack.
Set default layout on failure."
  (interactive)
  (condition-case nil
      (set-window-configuration (pop cg-layout-snapshots))
    (error (my-set-default-window-layout)))
  ;; prevent any further splitting
  (set-frame-parameter nil 'unsplittable t))

(global-set-key [f1] 'cg-layout-pop-snapshot)
(global-set-key [S-f1] 'cg-layout-push-snapshot)

(with-eval-after-load 'ediff
  (add-hook 'ediff-before-setup-hook #'cg-layout-push-snapshot)
  (add-hook 'ediff-quit-hook #'cg-layout-pop-snapshot))

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
