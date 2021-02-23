;; Load early-init.el on older Emacs versions such as the one on Ubuntu 20.4
(when (version< emacs-version "27.1")
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

;; Load all of our modules
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'cg-package)

(cg-package-require
 '(
   ;;+++
   cg-global
   ;; cg-app
   cg-layout
   cg-housekeeping
   cg-shared
   ;;+++
   ))
