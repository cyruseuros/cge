;;; cg-cpp.el --- C/C++ configuration -*- lexical-binding: t; -*-

(use-package cc-mode
  :defer t
  :config
  (c-add-style "valve"
               '((c-basic-offset . 4)
                 (c-commentonly-line-offset . 0)
                 (c-offsets-alist . ((statement-block-intro . +)
                                     (knr-argdecl-intro . +)
                                     (inline-open . 0)
                                     (block-open . -)
                                     (label . -)
                                     (statement-cont . +))))))



(use-package ggtags
  :defer t)

(use-package cmake-mode
  :defer t)

(use-package cperl-mode
  :defer t
  :init (setq cperl-invalid-face nil))

(provide 'cg-cpp)
;;; cg-cpp.el ends here
