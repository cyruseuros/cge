;; Load early-init.el on older Emacs versions such as the one on Ubuntu 20.4
(when (version< emacs-version "27.1")
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))

;; Load all of our modules
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(add-to-list 'load-path (concat user-emacs-directory "vendor"))
(require 'cg-package)

(cg-package-require
 '(
   ;;+++
   cg-core
   cg-layout
   cg-housekeeping
   cg-calc
   cg-app
   cg-treemacs
   cg-vc
   cg-theme
   cg-shell
   cg-fns ; TODO: Sort between modules
   cg-completion
   cg-scratch
   cg-el
   cg-nav ; TODO: Consider separating text and window/buffer nav
   ;;+++
   ))
