;;; cg-core.el -*- lexical-binding: t; -*-

(global-display-line-numbers-mode +1)
(global-auto-revert-mode +1)
(global-hl-line-mode +1)

(electric-indent-mode +1)
(transient-mark-mode +1)
(column-number-mode +1)
(save-place-mode +1)
(savehist-mode +1)

(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq user-full-name "Chris Green"
      tab-width 4
      visible-bell t
      mouse-scroll-delay 0
      auto-save-default t
      search-highlight t
      query-replace-highlight t
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

(provide 'cg-core)
