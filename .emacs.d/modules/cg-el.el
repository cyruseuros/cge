;;; cg-el.el -*- lexical-binding: t; -*-

(defun cg-el-indent-command ()
  "indent line, or region if mark active"
  (interactive)
  (if mark-active (indent-region (mark) (point) nil)
    (lisp-indent-line))
  (deactivate-mark))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
         ("\C-m" . newline-and-indent)
         ("\t" . cg-el-indent-command)
         ([M-mouse-2] . imenu)))

(provide 'cg-el)
;;; cg-el.el ends here
