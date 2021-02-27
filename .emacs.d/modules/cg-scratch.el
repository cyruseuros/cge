;;; cg-scratch.el -*- lexical-binding: t; -*-

;; Make scratch buffer unkillable
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'cg-scratch-kill-buffer))

(defun cg-scratch-kill-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))

  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'cg-scratch-kill-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'cg-scratch-kill-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)

(cg-scratch-kill-buffer)

(provide 'cg-scratch)
;;; cg-scratch.el ends here
