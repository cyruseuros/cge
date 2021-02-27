;;; cg-font-lock.el -*- lexical-binding: t; -*-

(setq font-lock-maximum-decoration t
      font-lock-maximum-size (* 1024 300)
      ;; NOTE: Deprecated, you might want to customize
      ;; `font-lock-highlighting-faces' in the future
      font-lock-maximum-decoration '((c-mode . 3) (c++-mode . 3)
				     (makefile-mode . 3) (cperl-mode . 3)
				     (lisp-mode .3 ))
      font-lock-face-attributes '((font-lock-keyword-face "black" "white" t nil nil )
	                          (font-lock-string-face "dark violet" "white" nil nil nil)
	                          (font-lock-comment-face "blue" "white" nil t nil)
	                          (font-lock-function-name-face "black" "yellow" nil nil nil)
	                          (font-lock-variable-name-face "orangered1" "white" nil nil nil)
	                          (font-lock-type-face "maroon" "white" nil nil nil))
      ;; (font-lock-reference-face "yellow4" "white" nil nil nil)
      ;; (font-lock-bracket-face "black" "slategray1" nil nil nil)
      ;; NOTE: `fast-lock-mode' is also deprecated hence disabled
      ;; font-lock-support-mode 'fast-lock-mode
      ;; fast-lock-cache-directories `(,(no-littering-expand-var-file-name "fast-lock/") ".")
      )

(provide 'cg-font-lock)
;;; cg-font-lock.el ends here
