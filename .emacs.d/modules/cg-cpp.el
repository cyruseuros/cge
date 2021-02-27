;;; cg-cpp.el --- C/C++ configuration -*- lexical-binding: t; -*-

(defun cg-cpp-clang-format-command ()
  "clang format buffer or region"
  (interactive)
  (if mark-active (clang-format-region (min (point) (mark)) (max (mark) (point)) nil)
    (clang-format-buffer))
  (deactivate-mark))

(use-package doxy-cpp
  :straight nil
  :commands (cpp-pretify))

(use-package cc-mode
  :defer t
  :init
  (setq c-basic-offset 4)
  (setq c-backslash-column 98)
  (setq c-noise-macros-names '( "constexpr" ) )
  :config
  (cpp-pretify)

  (c-add-style "valve"
               '((c-basic-offset . 4)
                 (c-commentonly-line-offset . 0)
                 (c-offsets-alist . ((statement-block-intro . +)
                                     (knr-argdecl-intro . +)
                                     (inline-open . 0)
                                     (block-open . -)
                                     (label . -)
                                     (statement-cont . +)))))
  (c-set-style "valve")
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)

  (add-hook 'c-mode-common-hook
            (lambda nil (setq-local
                         comment-column 60
                         fill-column 99
                         indent-tabs-mode t
                         tab-width 4)))
  :bind
  (:map c-mode-base-map
   ("\M-r" . gtags-find-rtag)
   ([C-M-tab] . clang-format-region)
   ("\t" . cg-fns-indent-command)
   ("\M-s" . cg-nav-visit-other-file )
   ;; ("\M-p" . visit-file-other-branch ) ; NOTE: Doesn't exist
   ([left] . cg-fns-hungry-backward-char)
   ([right] . cg-fns-hungry-forward-char)
   ([M-mouse-2] . imenu)
   ("\C-m" . cg-fns-newline-and-indent)))

(use-package ggtags
  :defer t)

(use-package cmake-mode
  :defer t)

(use-package cperl-mode
  :defer t
  :init (setq cperl-invalid-face nil))

(provide 'cg-cpp)
;;; cg-cpp.el ends here
