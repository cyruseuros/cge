;;; cg-theme.el -*- lexical-binding: t; -*-

(setq blink-matching-delay 0.5
      inhibit-startup-message t
      default-frame-alist (append '((vertical-scroll-bars nil)
                                    ;; add a little border to separate the menu bar etc.
                                    (internal-border-width . 2))
	                          default-frame-alist))

(set-cursor-color "#ff0000")
(set-face-foreground 'bold "magenta")
(set-face-background 'bold "white")
(set-face-foreground 'bold-italic "chocolate")
(set-face-foreground 'italic "blue")
(set-face-background 'region "skyblue")
(set-face-background 'isearch "seagreen3")
;;(set-face-underline-p 'font-lock-keyword-face t)
;;(set-face-foreground 'font-lock-string-face "dark violet")
;;(setq font-lock-keyword-face 'underline)

(defun cg-theme-align ()
  (interactive)
  (if mark-active (put-text-property
                   (min (point) (mark)) (max (mark) (point))
                   'display `(space . (:align-to 20)))))

(add-hook 'minibuffer-setup-hook 'cg-theme-minibuffer-setup)

(defun cg-theme-minibuffer-setup ()
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.5 ))))

(provide 'cg-theme)
;;; cg-theme.el ends here
