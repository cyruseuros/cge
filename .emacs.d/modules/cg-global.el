(global-display-line-numbers-mode +1)
(global-auto-revert-mode +1)
(global-hl-line-mode +1)

(column-number-mode +1)
(save-place-mode +1)
(savehist-mode +1)

(setq tab-width 4
      visible-bell t
      auto-save-default t
      hscroll-step 8
      truncate-lines t
      find-file-visit-truename t
      frame-title-format "%b - Emacs"
      icon-title-format "%b - Emacs")

(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
                ("\\.cc$" . c++-mode)
                ("\\.inl$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.hlsl$" . c++-mode)
                ("\\.fxc$" . c++-mode)
                ("\\.vfx$" . c++-mode)
                ("\\.c$"  . c++-mode)   ; to edit C code
                ("\\.h$"  . c++-mode)   ; to edit C code
                ("\\.db$" . sql-mode)
                ("\\.pl$" . cperl-mode)
                ("\\.rls" . lisp-mode)
                ("\\.cmd$" . cmd-mode)
                ("\\.bat$" . cmd-mode)
                ("makefile" . makefile-mode))
	      auto-mode-alist))

(provide 'cg-global)
