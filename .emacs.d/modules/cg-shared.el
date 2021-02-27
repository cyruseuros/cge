
;; *cg* c-backspace = del word backwards
(global-set-key [C-backspace] 'backward-kill-word)
;; *cg* automagic complete in minibuffer

(put 'narrow-to-region 'disabled nil)



(defun my-set-selective-display ()
  "set selective display to nil or current column"
  (interactive)
  (if selective-display
      (set-selective-display nil)
    (set-selective-display (+ (current-column) 1)))
  )

(global-set-key "\C-x$" 'my-set-selective-display)
;; *cg* enable horizontal scroll and set step to 8

;; *cg* make scratch buffer unkillable
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))



(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))

  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)

(kill-scratch-buffer)

;;(set-face-underline-p 'font-lock-keyword-face t)
;;(set-face-foreground 'font-lock-string-face "dark violet")
;;(setq font-lock-keyword-face 'underline)

(set-face-background 'region "skyblue")
;; *cg* change shell to always go to bottom when input is typed
(setq comint-scroll-to-bottom-on-input t)
(setq comint-show-maximum-output t)

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

(defun my-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.5 ))))

(add-hook 'shell-mode-hook
	  (function
	   (lambda ()
	     ;; (face-remap-add-relative 'default '((:foreground "#000000" ) ) )
	     (face-remap-add-relative 'default '((:background "#CCCCFF" ) ) )
	     )
	   )
	  )

(setq win32-swap-mouse-buttons t)

;; *cg* try to get rid of -i switch when starting shell
(setq explicit-cmd.exe-args " ")
(setq explicit-csh-args " ")

;; *cg* do smooth scroll rather than jump
(setq scroll-step 1)

;; *cg* don't make downarrow at end add more lines
(setq next-line-add-newlines nil)


;; Dont show the GNU splash screen
(setq inhibit-startup-message t)


(defun activate-next-window ()
  "activate the next window"
  (interactive)
  (select-window (next-window))
  )


(defun activate-next-frame ()
  "activate the next frame"
  (interactive)
  (focus-frame (next-frame))
  )

(global-set-key "\e\C-N" 'activate-next-window)
(global-set-key "\C-N" 'activate-next-frame)

;; *cg* I keep hitting c-x c-k when I mean c-x k
(global-set-key "\C-x\C-k" 'kill-buffer)

;;*cg* shift tab=dabbrev-expand
(global-set-key [backtab] 'dabbrev-expand)
(global-set-key [S-tab] 'dabbrev-expand)

;; *cg* turn on font-lock mode for c files and select indent style
;; *cg* and make ctrl-alt-y bury code
;; *cg* make alt-right-mouse pop up c file


(defun clang-format-command ()
  "clang format buffer or region"
  (interactive)
  (if mark-active (clang-format-region (min (point) (mark)) (max (mark) (point)) nil)
    (clang-format-buffer))
  (deactivate-mark)
  )


(require 'prettycpp)


(defun set-c-preferences ()
  "set up defaults for c and c++ mode"
  (setq c-noise-macros-names '( "constexpr" ) )
  (cpp-prettify)
  (setq comment-column 60)
  (setq fill-column 99)
  (setq c-backslash-column 98)
  (font-lock-mode t)
  (c-set-style "valve")
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (setq indent-tabs-mode t)
  (setq tab-width 4 )
  (setq c-basic-offset 4)
                                        ;  (gtags-mode 1)
  (define-key c-mode-map "\C-m" 'cg-fns-newline-and-indent)
  (local-set-key "\M-r" 'gtags-find-rtag)
  (local-set-key [C-M-tab] 'clang-format-region)
  (local-set-key "\t" 'cg-fns-indent-command)
  (local-set-key "\M-s" 'visit-other-file )
  (local-set-key "\M-p" 'visit-file-other-branch )
  (local-set-key [left] 'cg-fns-hungry-backward-char)
  (local-set-key [right] 'cg-fns-hungry-forward-char)
                                        ;  (setq font-lock-keywords (append '(("hello" . font-lock-reference-face)) font-lock-keywords))
  (local-set-key [M-mouse-2] 'imenu))

(defun set-make-preferences ()
  "set prefs for make mode"
  (local-set-key "\C-c\C-c" 'comment-region)
  (font-lock-mode t)
  )

(defun cg-fns-indent-command-lisp ()
  "indent line, or region if mark active"
  (interactive)
  (if mark-active (indent-region (mark) (point) nil)
    (lisp-indent-line))
  (deactivate-mark))

(defun set-lisp-preferences ()
  "set up defaults for lisp mode"
  (font-lock-mode t)
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (local-set-key [left] 'cg-fns-hungry-backward-char)
  (local-set-key [right] 'cg-fns-hungry-forward-char)
  (local-set-key "\t" 'cg-fns-indent-command-lisp)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key [M-mouse-2] 'imenu))


(load-library "p4")
(electric-indent-mode 1)



(defun set-perl-preferences ()
  "set up defaults for perl mode"
  (font-lock-mode t)
  (local-set-key [left] 'cg-fns-hungry-backward-char)
  (local-set-key [right] 'cg-fns-hungry-forward-char)
  (local-set-key "\C-c\C-c" 'comment-region)
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent))


(global-set-key [kp-space] 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [prior] 'cg-fns-better-pageup)
(global-set-key [next] 'cg-fns-better-pagedown)
(global-set-key [C-prior] 'beginning-of-buffer)
(global-set-key [C-next] 'end-of-buffer)
(global-set-key "\M-[" 'push-position)
(global-set-key "\M-]" 'pop-position)
(global-set-key [C-kp-prior] 'beginning-of-buffer)
(global-set-key [C-kp-next] 'end-of-buffer)


(defun push-position ()
  (interactive)
  (push-mark nil nil nil))

(defun pop-position ()
  (interactive)
  (if (null (mark t))
      (error "No mark set in this buffer")
    (goto-char (mark t))
    (pop-mark)))


(defun visit-other-file ()
  (interactive)
  (let ((fname (buffer-file-name))
	(done 0 )
	(patlist '(
		   ("\\.cpp$" ".h" )		;visit .h from .cpp
		   ("\\.h$" ".cpp" )		;visit cpp from .h
		   ) )
	hname)
    ;; If the current buffer is not visiting a file, signal an error
    (unless (stringp fname)
      (error "No file associated with the buffer %s" (buffer-name)))
    (message fname)
    (while (and (eq done 0 ) patlist )
      (let ((elt (car patlist)))
	(message "elt=%S" elt)
	(setq hname (replace-regexp-in-string (car elt) (car (cdr elt)) fname ) )
	(message "replaced %S with %S" fname hname )
	(if (and (not (string= hname fname) ) (file-readable-p hname) )
	    (progn
	      (find-file hname)
	      (setq done 1) ) )
	(setq patlist (cdr patlist) ) ))
    )
  )


;;*CG* Alt-B=select buffer
(defvar buffer-menu-exclude-regexp "^ \\|\\.bbdb\\|\\*BBDB\\|tags\\|\\*Ediff Registtry"
  "Regexp matching buffer names which should not be listed in buffer
   pop up menu"
  )

(defun kb-buffer-menu ()
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
			   (if (not (string-match buffer-menu-exclude-regexp
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


(global-set-key "\M-b" 'kb-buffer-menu)

(add-hook 'makefile-mode-hook 'turn-on-font-lock)


(add-hook 'perl-mode-hook
	  (function
	   (lambda ()
	     (set-perl-preferences)
	     )
	   )
	  )

(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda ()
	     (set-lisp-preferences)
	     )
	   )
	  )

(add-hook 'c-mode-hook
	  (function
	   (lambda ()
	     (set-c-preferences)
	     )
	   )
	  )

(add-hook 'c++-mode-hook
	  (function
	   (lambda ()
	     (set-c-preferences)
	     )
	   )
	  )

(scroll-bar-mode -1)

;; *cg* rebind some keypad keys
(global-set-key [insert] 'yank)

;; *cg* set ALT-K to delete a block
(global-set-key "\M-k" 'kill-sexp)
;; *cg* ESC-DEL = kill word
(global-set-key [ESC-delete] 'kill-word)

;;*cg* make keypad-* do undo like brief
(global-set-key [kp-multiply] 'undo)
;;*cg* make shift-insert fo pm-paste
(global-set-key [kp-0] 'pm-paste)


;;(autoload 'turn-on-fast-lock "fast-lock"
;;  "Unconditionally turn on Fast Lock mode.")
;;(add-hook 'font-lock-mode-hook 'turn-on-fast-lock)


(setq mark-mode 0)
;; *cg* add highlighting mode
(require 'vismark)

;; Support for marking a rectangle of text with highlighting.
;; this integrates with the rectangle support in vismark.el
(require 'rect-mark)
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)

(global-set-key "\M-c" 'rm-set-mark)

;; Don't like the idea of checking out not-found files from RCS
(remove-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)



;; If you check out previous revisions of files using the version control
;; package, they get named "filename.~revision~".  auto-mode-alist supports
;; a way to tell it extension-pieces it should ignore before evaluating
;; the mode (by using a three element list, not a cons).  The second element
;; is a function to call, and the third must be true.  (lambda) is a convenient
;; no-op.  The tricky part is that Emacs pre-excludes the trailing
;; tilde so that backup files are considered the right mode.  Hence
;; the correct pattern to supply is:
;;     "period, tilde, any digits or periods, end-of-name".
(setq auto-mode-alist
      (append '(("\\.~[0-9.]*\\'" (lambda) t)
		) auto-mode-alist))



;; Turn on feature where current search-match is highlighted,
;; and use seagreen3 to highlight it
(setq search-highlight t)
(setq query-replace-highlight t)
(make-face 'isearch)
(set-face-background 'isearch "seagreen3")


;; crank up that mouse scroll speed
(setq mouse-scroll-delay 0)

;; mail customizations:
(setq user-full-name "Chris Green")



(global-font-lock-mode)

(define-derived-mode cmd-mode text-mode "CMD"
  "Major mode for .CMD and .BAT files"

  (setq font-lock-keywords '(
			     ("^rem\\s.*" . font-lock-comment-face)
			     )
	)
  (font-lock-mode t)
  )


(put 'eval-expression 'disabled nil)




(put 'upcase-region 'disabled nil)

;;
;; Scrolling
;;

(defun emx-scroll-line-up (arg)
  "Scroll up by one line.
  With argument, do this that many times."
  (interactive "p")
  (scroll-up arg))

(defun emx-scroll-line-down (arg)
  "Scroll down by one line.
  With argument, do this that many times."
  (interactive "p")
  (scroll-down arg))

(global-set-key [C-up] 'emx-scroll-line-down)
(global-set-key [C-down] 'emx-scroll-line-up)


(put 'downcase-region 'disabled nil)
