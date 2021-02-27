;;; cg-nav.el -*- lexical-binding: t; -*-

(setq scroll-step 1 ; Smooth scroll
      ;; Don't make downarrow at end add more lines
      next-line-add-newlines nil
      highlight-nonselected-windows nil
      win32-swap-mouse-buttons t)

(defun cg-nav-push-position ()
  (interactive)
  (push-mark nil nil nil))

(defun cg-nav-pop-position ()
  (interactive)
  (if (null (mark t))
      (error "No mark set in this buffer")
    (goto-char (mark t))
    (pop-mark)))

(defun cg-nav-next-window ()
  "activate the next window"
  (interactive)
  (select-window (next-window)))

(defun cg-nav-next-frame ()
  "activate the next frame"
  (interactive)
  (focus-frame (next-frame)))

(defun cg-nav-set-selective-display ()
  "set selective display to nil or current column"
  (interactive)
  (if selective-display
      (set-selective-display nil)
    (set-selective-display (+ (current-column) 1))))

(defvar cg-nav-last-region-type 'region
  "set to either 'region or 'rectangle based upon last cut or copy")

(defun cg-nav-shift-down ()
  "drop mark if not active, and move down"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (next-line 1))

(defun cg-nav-shift-up ()
  "drop mark if not active, and move up"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (previous-line 1))

(defun cg-nav-shift-right ()
  "drop mark if not active, and move right"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (forward-char 1))

(defun cg-nav-shift-left ()
  "drop mark if not active, and move left"
  (interactive)
  (if (not mark-active) (set-mark (point)))
  (backward-char 1))

(defun cg-nav-togglemark ()
  "start or stop marking"
  (interactive)
  (if mark-active (deactivate-mark) (set-mark (point))))

(defun cg-nav-cut-cur-line ()
  "kill current line to scrap"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (forward-char 1)
    (kill-region (mark) (point))))

(defun cg-nav-minus-key ()
  "if mark active, kill text, else insert a -"
  (interactive)
  (if rm-mark-active
      (progn
	(rm-kill-region (mark) (point))
	(setq cg-nav-last-region-type 'rectangle))
    (if mark-active
	(progn
	  (kill-region (mark) (point))
	  (setq cg-nav-last-region-type 'region))
      (cg-nav-cut-cur-line))))

(defun cg-nav-plus-key ()
  "if mark active, copy text, else insert a +"
  (interactive)
  (if rm-mark-active
      (progn
	(rm-kill-ring-save (mark) (point))
	(rm-deactivate-mark)
	(setq cg-nav-last-region-type 'rectangle)
	)
    (if mark-active
	(progn
	  (copy-region-as-kill (mark) (point))
	  (deactivate-mark)
	  (setq cg-nav-last-region-type 'region)
	  )
      (cg-nav-copy-cur-line))))

(defun cg-nav-delete-key ()
  "if mark active, delete text, else delete a char"
  (interactive)
  (if rm-mark-active
      (progn
	(delete-rectangle (mark) (point))
	(setq cg-nav-last-region-type 'rectangle)
	(rm-deactivate-mark)
	)
    (if mark-active
	(progn
	  (delete-region (mark) (point))
	  (deactivate-mark)
	  (setq cg-nav-last-region-type 'region))
      (delete-char 1))))

(defun cg-nav-space-key ()
  "if col mark active, insert whitespace, else insert a space"
  (interactive)
  (if rm-mark-active
      (open-rectangle (mark) (point))
    (insert " ")))

(defun cg-nav-copy-cur-line ()
  "copy current line to scrap"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (forward-char 1)
    (copy-region-as-kill (mark) (point))))

(defun cg-nav-copy-cur-line-to-other-window ()
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
      (insert "\n"))))

(defun cg-nav-insert-key ()
  "insert either a rectangle or region based upon last cut or copy"
  (interactive)
  (if (eq cg-nav-last-region-type 'rectangle)
      (yank-rectangle)
    (yank)))

(defun cg-nav-visit-other-file ()
  (interactive)
  (let ((fname (buffer-file-name))
	(done 0 )
	(patlist '(("\\.cpp$" ".h" ) ; visit .h from .cpp
                   ("\\.h$" ".cpp" ) ; visit cpp from .h
		   ))
	hname)
    ;; If the current buffer is not visiting a file, signal an error
    (unless (stringp fname)
      (error "No file associated with the buffer %s" (buffer-name)))
    (message fname)
    (while (and (eq done 0 ) patlist )
      (let ((elt (car patlist)))
	(message "elt=%S" elt)
	(setq hname (replace-regexp-in-string (car elt) (car (cdr elt)) fname))
	(message "replaced %S with %S" fname hname )
	(if (and (not (string= hname fname) ) (file-readable-p hname))
	    (progn
	      (find-file hname)
	      (setq done 1)))
	(setq patlist (cdr patlist))))))

;;*CG* Alt-B=select buffer
(defvar cg-nav-buffer-menu-exclude-regexp "^ \\|\\.bbdb\\|\\*BBDB\\|tags\\|\\*Ediff Registry"
  "Regexp matching buffer names which should not be listed in buffer
   pop up menu")

(defun cg-nav-buffer-menu ()
  "Pop up a menu of buffers for selection with the keyboard
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive)
  (let ((menu
	 (list "Buffer Menu"
	       (cons "Select Buffer"
		     (let ((tail (buffer-list))
			   (maxbuf 0)
			   head)
		       (while tail
			 (or (eq ?\ (aref (buffer-name (car tail)) 0))
			     (setq maxbuf
				   (max maxbuf
					(length (buffer-name (car tail))))))
			 (setq tail (cdr tail)))
		       (setq tail (buffer-list))
		       (while tail
			 (let ((elt (car tail)))
			   (if (not (string-match cg-nav-buffer-menu-exclude-regexp
				                  (buffer-name elt)))
			       (setq head (cons
				           (cons
				            (format
				             (format "%%%ds  %%s%%s  %%s"
				                     maxbuf)
				             (buffer-name elt)
				             (if (buffer-modified-p elt)
				                 "*" " ")
				             (save-excursion
				               (set-buffer elt)
				               (if buffer-read-only "%" " "))
				             (or (buffer-file-name elt) ""))
				            elt)
				           head))))
			 (setq tail (cdr tail)))
		       (reverse head))))))
    (let ((buf (x-popup-menu (list '(15 3) (selected-window)) menu)))
      (if buf (switch-to-buffer buf)))))

(defun cg-nav-scroll-line-up (arg)
  "Scroll up by one line.
  With argument, do this that many times."
  (interactive "p")
  (scroll-up arg))

(defun cg-nav-scroll-line-down (arg)
  "Scroll down by one line.
  With argument, do this that many times."
  (interactive "p")
  (scroll-down arg))

(bind-keys
 ("\C-x$" . cg-nav-set-selective-display)
 ("\C-x\C-k" . kill-buffer)
 ([C-backspace] . backward-kill-word)
 ("\e\C-N" . cg-nav-next-window)
 ("\C-N" . cg-nav-next-frame)
 ;;(" "  . cg-nav-space-key)
 ([S-down] . cg-nav-shift-down)
 ([S-up] . cg-nav-shift-up)
 ([S-left] . cg-nav-shift-left)
 ([S-right] . cg-nav-shift-right)
 ("\M-m" . cg-nav-togglemark)
 ([kp-subtract] . cg-nav-minus-key)
 ([kp-add] . cg-nav-plus-key)
 ([delete] . cg-nav-delete-key)
 ([insert] . cg-nav-insert-key)
 ([C-+] . cg-nav-copy-cur-line)
 ([M-C-+] . cg-nav-copy-cur-line-to-other-window)
 ("\M-b" . cg-nav-buffer-menu)
 ;; rebind some keypad keys
 ([insert] . yank)
 ;; set ALT-K to delete a block
 ("\M-k" . kill-sexp)
 ;; ESC-DEL = kill word
 ([ESC-delete] . kill-word)
 ;; make keypad-* do undo like brief
 ([kp-multiply] . undo)
 ;; make shift-insert of pm-paste
 ;; ([kp-0] . pm-paste) ; NOTE: Unknown function
([C-up] . cg-nav-scroll-line-down)
([C-down] . cg-nav-scroll-line-up))

(use-package id-select
  :straight nil)

;; Support for marking a rectangle of text with highlighting.
;; this integrates with the rectangle support in vismark.el
(use-package rect-mark
  :bind
  (("\M-c" . rm-set-mark)
   ([S-down-mouse-1] . rm-mouse-drag-region)
   :map ctrl-x-map
   ("r\C-@" . rm-set-mark)
   ([?r ?\C-\ ] . rm-set-mark)
   ("r\C-x" . rm-exchange-point-and-mark)
   ("r\C-w" . rm-kill-region)
   ("r\M-w" . rm-kill-ring-save)))

(provide 'cg-nav)
;;; cg-nav.el ends here
