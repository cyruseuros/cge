;;; cg-package.el --- Packaging setup -*- lexical-binding: t; -*-

;; Bootrstrap straight.el. Since we are vendoring all dependencies, this is
;; basically a "build-time" dependency (compile-time would be misleading). This
;; way nothing ever changes under us, but if we choose to update things, the
;; procedure isn't manual. Further, if we do lose our dependencies, we can
;; reproduce them down to their git hash.
(defvar bootstrap-version)
(setq straight-use-package-by-default t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Nice and tidy way to configure our packages
(straight-use-package 'use-package)

(defun cg-package-require (packages)
  (dolist (pkg packages)
    (require pkg)))

(defun cg-package-autoload (pkg &rest funcs)
  "Autlod multiple function."
  (dolist (func funcs)
    (autoload func (symbol-name pkg))))

(provide 'cg-package)
;;; cg-package.el ends here
