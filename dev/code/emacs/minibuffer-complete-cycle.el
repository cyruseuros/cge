
;;; RCS $Id: minibuffer-complete-cycle.el,v 1.1 1997/10/15 21:02:45 kevinr Exp $

;;; Description:
;;; 
;;; The `minibuffer-complete' command, bound by default to TAB in the
;;; minibuffer completion keymaps, displays the list of possible
;;; completions when no additional characters can be completed.
;;; Subsequent invocations of this command cause the window displaying
;;; the *Completions* buffer to scroll.
;;; 
;;; This package advises the `minibuffer-complete' command so that each
;;; of the possible completions is instead inserted in turn into the
;;; minibuffer and highlighted in the *Completions* buffer.  The window
;;; displaying the possible completions is scrolled only if necessary.
;;; This enhancement is enabled or disabled by setting or unsetting the
;;; `minibuffer-complete-cycle' variable.

;;; Acknowledgments:
;;; 
;;; The technique of deleting the minibuffer contents, then (for file
;;; name completion) inserting the directory component of the initial
;;; input, and then inserting the completion string itself is based on
;;; cycle-mini.el (1.03) by Joe Reiss <jreiss@vt.edu>.

;;; Copyright:
;;; 
;;; Copyright © 1997 Kevin Rodgers
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 
;;; My employer (Information Handling Services) has not disclaimed any
;;; copyright interest in minibuffer-complete-cycle.el.
;;; 
;;; Kevin Rodgers <kevinr@ihs.com>          Lead Software Engineer
;;; Information Handling Services           Electronic Systems Development
;;; 15 Inverness Way East, M/S A201         GO BUFFS!
;;; Englewood CO 80112-5776 USA             1+ (303) 397-2807[voice]/-2244[fax]

;;; LCD Archive Entry:
;;; 
;;; minibuffer-complete-cycle|Kevin Rodgers|kevinr@ihs.com|
;;; Cycle through the *Completions* buffer|
;;; $Date: 1997/10/15 21:02:45 $|$Revision: 1.1 $||


;;; Package interface:
(provide 'minibuffer-complete-cycle)


;;; User options:
(defvar minibuffer-complete-cycle nil
  "*If non-nil, `minibuffer-complete' cycles through the possible completions.")
(put 'minibuffer-complete-cycle 'variable-interactive
     "XEnable cycling through *Completions*? (t or nil): ")


;;; Internal variables:
(defvar mcc-completion-begin nil
  "The point in the *Completions* buffer of the last selected completion.")
(defvar mcc-completion-end nil
  "The point in the *Completions* buffer of the last selected completion.")

(defvar mcc-overlay (let ((overlay (make-overlay 1 1)))
		      (overlay-put overlay 'face 'secondary-selection)
		      overlay)
  "The overlay used to highlight the *Completions* buffer.")


;;; Commands:
(defadvice minibuffer-complete (around cycle activate)
  "If `minibuffer-complete-cycle' is non-nil, then instead of just scrolling
the window of possible completions, insert each one in turn in the minibuffer
and highlight it in the *Completions* buffer."
  ;; Emulate Fminibuffer_complete:
  (if (and minibuffer-complete-cycle
	   (not (eq last-command this-command)))
      (setq minibuffer-scroll-window nil))
  (if (and minibuffer-complete-cycle
	   minibuffer-scroll-window
	   (window-live-p minibuffer-scroll-window))
      ;; Delete the current completion, then insert and highlight the
      ;; next completion:
      (let ((input (buffer-substring (point-min) (point-max))))
	(erase-buffer)
	(if (eq minibuffer-completion-table 'read-file-name-internal)
	    (insert (file-name-directory input)))
	(insert (mcc-completion-string))
	(mcc-highlight-completion))
    ;; Reset the mcc variables and proceed normally:
    (progn
      (setq mcc-completion-begin nil
	    mcc-completion-end nil)
      ad-do-it)))


;;; Functions:
(defun mcc-completion-string ()
  "Return the next completion."
  (let ((completion-buffer (get-buffer "*Completions*")))
    ;; Verify the buffer and window configuration:
    (or (eq completion-buffer (window-buffer minibuffer-scroll-window))
	(error "minibuffer-scroll-window isn't displaying \
the *Completions* buffer"))
    (save-excursion
      (set-buffer completion-buffer)
      ;; Find the beginning of the completion (see Fdisplay_completion_list):
      (if (and mcc-completion-begin  mcc-completion-end
	       (< mcc-completion-end (point-max)))
	  (progn
	    (goto-char mcc-completion-begin)
	    (if (= (current-column) 0)
		(progn
		  (move-to-column 35)
		  ;; Handle wide completions:
		  (if (not (char-equal (char-after (1- (point))) ? )) ; SPC
		      (progn
			(skip-chars-forward "^ \t")
			(skip-chars-forward " \t"))))
	      (forward-line)))
	(progn
	  (goto-char (point-min))
	  (search-forward "Possible completions are:\n")))
      (setq mcc-completion-begin (point))
      ;; Find the end of the completion (see Fdisplay_completion_list):
      (if (= (current-column) 0)
	  (progn
	    (move-to-column 35)
	    ;; Handle wide completions:
	    (if (char-equal (char-after (1- (point))) ? ) ; SPC
		(skip-chars-backward " \t")
	      (skip-chars-forward "^ \t")))
	(end-of-line))
      (setq mcc-completion-end (point))
      ;; Return the next completion it:
      (buffer-substring mcc-completion-begin mcc-completion-end))))

(defun mcc-highlight-completion ()
  "Highlight the current completion."
  (let ((completion-buffer (window-buffer minibuffer-scroll-window)))
    (move-overlay mcc-overlay mcc-completion-begin mcc-completion-end
		  completion-buffer) 
    ;; ... and make sure its visible (see Fminibuffer-complete):
    (save-excursion
      (set-buffer completion-buffer)	; why is this necessary?
      (or (pos-visible-in-window-p mcc-completion-begin
				   minibuffer-scroll-window)
	  (scroll-other-window nil)))))

;;;; minibuffer-complete-cycle.el ends here
