;;; cg-calc.el --- Configure Calc -*- lexical-binding: t; -*-

;; All functions we previously autoloaded by hand are now autoloaded by default.
(use-package calc
  :bind ("\e#" . calc-dispatch))

(provide 'cg-calc)
;;; cg-calc.el ends here
