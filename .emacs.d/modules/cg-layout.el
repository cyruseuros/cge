;;; cg-layout.el --- Personal window management -*- lexical-binding: t; -*-

;; Breakpoints in inches/ppi as needed
(defvar cg-layout-breakpoint-alist
  '((phone . 7)
    (tablet . 11)
    (laptop . 16)
    (hidpi . 200))
  "Breakpoints in inches/ppi.")

(defun cg-layout-inches (mm)
  (/ mm 25.4))

(defun cg-layout-hypotenuse (opp adj)
  (sqrt (+ (expt opp 2) (expt adj 2))))

(let* ((attrs (car (display-monitor-attributes-list)))
       (size (alist-get 'mm-size attrs))
       (pixels (alist-get 'geometry attrs))
       (width (cg-layout-inches (car size)))
       (height (cg-layout-inches (cadr size)))
       (diag (cg-layout-hypotenuse width height))
       (ppi (/ (nth 3 pixels) width)))
  (defvar cg-layout-hidpi (> ppi (alist-get 'hidpi cg-layout-breakpoint-alist)))
  (defvar cg-layout (cond ((< diag (alist-get 'phone cg-layout-breakpoint-alist)) 'phone)
                          ((< diag (alist-get 'tablet cg-layout-breakpoint-alist)) 'tablet)
                          ((< diag (alist-get 'laptop cg-layout-breakpoint-alist)) 'laptop)
                          ((> diag (alist-get 'laptop cg-layout-breakpoint-alist)) 'desktop))))

(defvar cg-layout-alist
  '((desktop (:recipe (split-window-right))
             (:recipe (progn (split-window-right) (balance-windows)) :focus t)
             (:recipe (split-window-below) :buffers ("\\*shell\\*" "\\*compilation\\*"))
             (:recipe (split-window-below) :buffers ("\\*Completions\\*"))
             (:recipe (frame-first-window) :buffers t)))
  "Alist of layout names (one of phone/tablet/laptop/desktop).")

(defun cg-layout-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

(defun cg-layout-frame ()
  (interactive)
  (set-frame-parameter nil 'unsplittable t)
  (set-frame-height nil 105)
  (set-frame-width nil 538)
  (set-frame-position (selected-frame) 5 6))

(defun cg-layout-apply (layout-list)
  "Take one LAYOUT-LIST from `cg-layout-alist' and apply it."
  (cg-layout-frame)
  (let ((focused-window nil))
    (dolist (window-plist layout-list)
      (let* ((window (eval (plist-get window-plist :recipe)))
             (buffer-matchers (plist-get window-plist :buffers))
             (buffer-catcher (lambda (buffer alist)
                               ;; don't change focus is buffer is visible
                               (when (not (member buffer (cg-layout-visible-buffers)))
                                 (set-window-buffer window buffer))))
             (focus (plist-get window-plist :focus)))
        (when focus
          (setq focused-window window))
        ;; we have a list of things to catch
        (if (listp buffer-matchers)
            (dolist (buffer-matcher buffer-matchers)
              (add-to-list 'display-buffer-alist `(,buffer-matcher (,buffer-catcher))))
          ;; if t make it the default window
          (when buffer-matchers
            (setq display-buffer-base-action `(,buffer-catcher))))))
    (when focused-window
      (select-window focused-window))))

(defun cg-layout-refresh ()
  "Apply selected layout."
  (interactive)
  (cg-layout-apply (or (alist-get cg-layout cg-layout-alist)
                       (alist-get 'desktop cg-layout-alist))))

(cg-layout-refresh)

(use-package windmove
  :init (windmove-default-keybindings 'meta)
  :commands (windmove-find-other-window))

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

;; TODO: Check if this function actually works
(defun cg-layout-line-up ()
  (interactive)
  (when mark-active
    (put-text-property
     (min (point) (mark)) (max (mark) (point))
     'display `( space . ( :align-to 20 )))))

(provide 'cg-layout)
;;; cg-layout.el ends here
