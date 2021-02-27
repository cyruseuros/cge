;;; cg-vc.el -*- lexical-binding: t; -*-


;; If you check out previous revisions of files using the version control
;; package, they get named "filename.~revision~".  auto-mode-alist supports
;; a way to tell it extension-pieces it should ignore before evaluating
;; the mode (by using a three element list, not a cons).  The second element
;; is a function to call, and the third must be true.  (lambda) is a convenient
;; no-op.  The tricky part is that Emacs pre-excludes the trailing
;; tilde so that backup files are considered the right mode.  Hence
;; the correct pattern to supply is:
;;     "period, tilde, any digits or periods, end-of-name".
(add-to-list 'auto-mode-alist '("\\.~[0-9.]*\\'" (lambda) t))

;; Don't like the idea of checking out not-found files from RCS
(remove-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)

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

(use-package p4
  :defer t)

(use-package vc-p4
  :straight nil
  :defer t)

(provide 'cg-vc)
;;; cg-vc.el ends here
