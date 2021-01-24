(defun hungry-forward-char ()
    "move forward, skipping white-space"
    (interactive)
    (if (and (or (eq (preceding-char) 9) (eq (preceding-char) 32)
		 (eq (preceding-char) 10))
	     (or (eq (following-char) 9) (eq (following-char) 32)))
	(skip-chars-forward " \t")
      (forward-char 1)))

(defun hungry-backward-char ()
    "move backward, skipping white-space"
    (interactive)
    (if (or (eq (preceding-char) 9) (eq (preceding-char) 32))
	(skip-chars-backward " \t")
      (backward-char 1)))

(defun my-indent-command ()
  "indent line, or region if mark active"
  (interactive)
  (if mark-active (indent-region (min (point) (mark)) (max (mark) (point)) nil)
    (c-indent-command))
  (deactivate-mark)
  )
  
(defun indent-and-newline-and-indent ()
  "indent curline, add newline, and indent new line"
  (interactive)
  (c-indent-command)
  (newline-and-indent)
  )

(defun current-line ()
  "return current line #"
  ( let ( ( retval 0 ) )
	(save-excursion
      (beginning-of-line)
      (setq retval (count-lines 1 (point)))
      )
	retval
	)
)

(defun current-window-line ()
  "return current line # in window"
  (count-lines (window-start) (point))
)

(defun better-pageup ()
  "smart page up which properly restores position."
  (interactive)
  ( let ( ( nlines  (- (window-height) 2) )
		  ( recenter-line (current-window-line ) ) )
	(if (> (current-line) nlines)
		(previous-line nlines)
      (beginning-of-buffer))
	(if (not selective-display)
		(recenter recenter-line)
	  ) )
  )

(defun better-pagedown ()
  "smart page down which properly restores position."
  (interactive)
  ( let ( ( nlines  (- (window-height) 2) )
		  ( recenter-line (current-window-line ) ) )
	(next-line nlines)
	(if (not selective-display)
		(recenter recenter-line))
	(recenter recenter-line)
	)
  )


(provide 'cg-fns)
