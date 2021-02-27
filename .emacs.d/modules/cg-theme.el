;;; cg-theme.el -*- lexical-binding: t; -*-

(set-cursor-color "#ff0000")

(set-face-foreground 'bold "magenta")
(set-face-background 'bold "white")
(set-face-foreground 'bold-italic "chocolate")
(set-face-foreground 'italic "blue")
;; (make-face 'extra)

(defun cg-theme-align ()
  (interactive)
  (if mark-active (put-text-property
                   (min (point) (mark)) (max (mark) (point))
                   'display `(space . (:align-to 20)))))

(setq blink-matching-delay 0.5
      default-frame-alist (append '((vertical-scroll-bars nil)
                                    ;; add a little border to separate the menu bar etc.
                                    (internal-border-width . 2))
	                          default-frame-alist))

(provide 'cg-theme)
;;; cg-theme.el ends here
