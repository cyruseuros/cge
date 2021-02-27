;;; cg-app.el -*- lexical-binding: t; -*-

(defvar cg-app-run-command "cd d:\\dev\\code\\demo && demo -novr"
  "Command used to run an app demo.")

(defun cg-app-compile ()
  (interactive)
  (save-some-buffers 1)
  (recompile))

(defun cg-app-run ()
  (interactive)
  (shell)
  (insert run-command)
  (comint-send-input nil t))

(use-package compile
  :commands (recompile)
  :bind ([f9] . cg-app-compile)
  :init
  (setq compile-command "msbuild d:\\dev\\code\\code.sln -maxcpucount:10 /p:Configuration=RelWithDebInfo 2>&1 | perl d:\\dev\\code\\tools\\msbuild_filter.pl"
        compilation-scroll-output 'first-error)
  :config
  (add-hook 'compilation-mode-hook
            #'visual-line-mode))

(provide 'cg-app)
;;; cg-app.el ends here
