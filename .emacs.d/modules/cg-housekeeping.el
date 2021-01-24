;;; cg-housekeeping.el --- Keep our .emacs.d nice and tidy -*- lexical-binding: t; -*-

(use-package no-littering
  :config
  ;; don't keep litter files in recentf list
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; keep auto-save files out of sight
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; keep gui configuration in the etc/ directory
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'cg-housekeeping)
;;; cg-housekeeping.el ends here
