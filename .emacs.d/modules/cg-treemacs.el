;;; cg-treemacs.el -*- lexical-binding: t; -*-

(defun treemacs-custom-filter (file _)
  (or
   ;;(s-starts--with? "INSTALL." file)
   (s-ends-with? ".log" file)
   (s-ends-with? ".vs" file)
   (s-ends-with? ".dir" file)
   (s-ends-with? ".elc" file)
   (s-ends-with? ".dat" file)
   (s-ends-with? ".sln" file)
   (s-ends-with? ".vcxproj" file)
   (s-ends-with? ".user" file)
   (s-ends-with? "CMakeCache.txt" file)
   (s-ends-with? ".json" file)
   (s-ends-with? "CMakeFiles" file)
   (s-ends-with? ".cmake" file)
   (s-ends-with? "Makefile" file)
   (s-ends-with? ".filters" file)
   ;;(s-starts--with? "PACKAGE." )
   ))

(use-package treemacs
  :defer t
  :config
  ;; TODO: Scale this depending on `display-monitor-attributes-list'
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-directory-name-transformer    #'identity
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-extension-regex          treemacs-last-period-regex-value
        treemacs-file-follow-delay             0.2
        treemacs-file-name-transformer         #'identity
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-move-forward-on-expand        nil
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-position                      'left
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-user-mode-line-format         nil
        treemacs-user-header-line-format       nil
        treemacs-width                         35
        treemacs-workspace-switch-cleanup      nil)
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-icons-dired
  :disabled t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(provide 'cg-treemacs)
;;; cg-treemacs.el ends here
