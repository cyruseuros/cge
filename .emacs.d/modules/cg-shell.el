;;; cg-shell.el -*- lexical-binding: t; -*-

;; Change shell to always go to bottom when input is typed
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-show-maximum-output t
      ;; Try to get rid of -i switch when starting shell
      explicit-cmd.exe-args " "
      explicit-csh-args " ")

(defun cg-shell-start-or-switch ()
  "select shell and move to bottom"
  (interactive)
  (shell)
  (end-of-buffer))

(add-hook 'shell-mode-hook
          (function
           (lambda ()
             ;; (face-remap-add-relative 'default '((:foreground "#000000")))
             (face-remap-add-relative 'default '((:background "#CCCCFF" ))))))

(use-package batch-mode
  :defer t)

(global-set-key [f10] 'cg-shell-start-or-switch)

(provide 'cg-shell)
;;; cg-shell.el ends here
