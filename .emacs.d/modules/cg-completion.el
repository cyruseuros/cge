;;; cg-completion.el -*- lexical-binding: t; -*-

(setq completion-ignored-extensions
      '(".o" ".err" ".elc" "~" ".com" ".exe" ".lib" ".flb"
	".dll" ".obj" ".bak" ".ico" ".zip" ".zoo" ".arj" ".lzh" ))

(defun cg-completion-ido-insert-slash (arg)
  "insert a slash"
  (interactive "p")
  (insert "/" ) )

(defun cg-completion-ido-keys ()
  "Add my keybindings for ido."
  (define-key ido-file-completion-map "\\" 'cg-completion-ido-insert-slash))

(defun cg-completion-ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(use-package ido
  :defer t
  :config (add-hook 'ido-setup-hook 'cg-completion-ido-keys))

(use-package minibuffer-complete-cycle
  :init (setq minibuffer-complete-cycle t))

(use-package dabbrev
  :bind
  (([backtab] . dabbrev-expand)
   ([S-tab] . dabbrev-expand))
  :init
  ;; Make dynamic abbrevs case-sensitive
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil))

(icomplete-mode +1)

(provide 'cg-completion)
;;; cg-completion.el ends here
