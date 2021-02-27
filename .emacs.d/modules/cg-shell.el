;;; cg-shell.el -*- lexical-binding: t; -*-

(defun cg-shell-start-or-switch ()
  "select shell and move to bottom"
  (interactive)
  (shell)
  (end-of-buffer))

(global-set-key [f10] 'cg-shell-start-or-switch)

(provide 'cg-shell)
;;; cg-shell.el ends here
