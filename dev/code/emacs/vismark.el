(transient-mark-mode 1)
(setq highlight-nonselected-windows nil)
(defvar last-region-type 'region
  "set to either 'region or 'rectangle based upon last cut or copy"
  )



(defun shift-down ()
  "drop mark if not active, and move down"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (next-line 1)
)

(defun shift-up ()
  "drop mark if not active, and move up"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (previous-line 1)
)

(defun shift-right ()
  "drop mark if not active, and move right"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (forward-char 1)
)

(defun shift-left ()
  "drop mark if not active, and move left"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (backward-char 1)
)

(defun ToggleMark ()
  "start or stop marking"
  (interactive)
  (if mark-active (deactivate-mark) (set-mark (point)))
)


(defun cut-cur-line ()
  "kill current line to scrap"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (forward-char 1)
    (kill-region (mark) (point))
    )
  )

(defun minus-key ()
  "if mark active, kill text, else insert a -"
  (interactive)
  (if rm-mark-active
      (progn
	(rm-kill-region (mark) (point))
	(setq last-region-type 'rectangle))
    (if mark-active
	(progn
	  (kill-region (mark) (point))
	  (setq last-region-type 'region)
	  )
      (cut-cur-line))))

(defun plus-key ()
  "if mark active, copy text, else insert a +"
  (interactive)
  (if rm-mark-active
      (progn
	(rm-kill-ring-save (mark) (point))
	(rm-deactivate-mark)
	(setq last-region-type 'rectangle)
	)
    (if mark-active 
	(progn
	  (copy-region-as-kill (mark) (point))
	  (deactivate-mark)
	  (setq last-region-type 'region)
	  )
      (copy-cur-line)
      )))

(defun delete-key ()
  "if mark active, delete text, else delete a char"
  (interactive)
  (if rm-mark-active
      (progn
	(delete-rectangle (mark) (point))
	(setq last-region-type 'rectangle)
	(rm-deactivate-mark)
	)
    (if mark-active
	(progn
	  (delete-region (mark) (point))
	  (deactivate-mark)
	  (setq last-region-type 'region)
	  )
      (delete-char 1)
      )
    )
  )

(defun space-key ()
   "if col mark active, insert whitespace, else insert a space"
   (interactive)
   (if rm-mark-active
       (open-rectangle (mark) (point))
     (insert " ")
     )
   )

(defun copy-cur-line ()
  "copy current line to scrap"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (forward-char 1)
    (copy-region-as-kill (mark) (point))
    )
  )


(defun copy-cur-line-to-other-window ()
  "copy current line to other-window"
  (interactive)
  (save-excursion
    (save-window-excursion
     (beginning-of-line)
     (set-mark (point))
     (end-of-line)
     (copy-region-as-kill (mark) (point))
     (other-window 1)
     (yank)
     (insert "\n");
     )
    )
)

(defun insert-key ()
  "insert either a rectangle or region based upon last cut or copy"
  (interactive)
  (if (eq last-region-type 'rectangle)
      (yank-rectangle)
    (yank)
    )
)



(global-set-key [S-down] 'shift-down)
(global-set-key [S-up] 'shift-up)
(global-set-key [S-left] 'shift-left)
(global-set-key [S-right] 'shift-right)
(global-set-key "\M-m" 'ToggleMark)
(global-set-key [kp-subtract] 'minus-key)
(global-set-key [kp-add] 'plus-key)
;;(global-set-key " " 'space-key)
(global-set-key [delete] 'delete-key)
(global-set-key [insert] 'insert-key)
(global-set-key [C-+] 'copy-cur-line)
(global-set-key [M-C-+] 'copy-cur-line-to-other-window)

(provide 'vismark)
