;;; cg-vc.el -*- lexical-binding: t; -*-

(use-package ediff
  ;; All the other functions you were autoloading already are
  :commands (ediff epatch epatch-buffer)
  :config
  ;; Your current window configuration breaks `ediff'. I can write a custom
  ;; `ediff-window-setup-function' but it just makes certain assumptions about
  ;; windows sizes and their relationship that simply aren't true in your case.
  ;; Changing the window configuration kind of defeates the purpose of not
  ;; disrupting your workflow. For now, I'll just make sure that `ediff' drops you
  ;; back right where you were before.
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-right
        ediff-default-variant 'default-A)
  (add-hook 'ediff-before-setup-hook #'cg-layout-push-snapshot)
  (add-hook 'ediff-quit-hook #'cg-layout-pop-snapshot))

(provide 'cg-vc)
;;; cg-vc.el ends here
