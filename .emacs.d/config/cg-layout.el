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

(cg-layout)

(provide 'cg-layout)
;;; cg-layout.el ends here
