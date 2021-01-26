;;; cg-app.el --- Configuration and operation utility functions -*- lexical-binding: t; -*-

(setq cg-app-run-command "cd d:\\dev\\code\\demo && demo -novr")

(defun cg-app-run ()
  (interactive)
  (shell)
  (insert cg-app-run-command)
  (comint-send-input nil t))

(defun cg-app-compile ()
  (interactive)
  (save-some-buffers 1 )
  (recompile))

(bind-keys ([f9] . cg-app-compile)
           ([f5] . cg-app-run))

(provide 'cg-util)
;;; cg-app.el ends here
