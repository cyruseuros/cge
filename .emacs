;;; .emacs -*- lexical-binding: t; -*-
;; run the shared one

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(defvar my-wip t
  "Whether work is in progress.
When nil paths are interpreted as raw strings, when t, assume all
directories are under `user-emacs-directory'.")

(defun my-dir (dir)
  "Strip disk names from paths if `my-wip'.
Also handle special case of the googledrive directory."
  (let ((drive-regex "[a-z]:/")
        (gdrive-regex "googledrive/emacs/"))
    (if my-wip
        (let* ((no-drive-dir (replace-regexp-in-string drive-regex "" dir))
               (no-gdrive-dir (replace-regexp-in-string gdrive-regex "" no-drive-dir))
               (bundled-dir (concat (file-name-directory load-file-name) no-gdrive-dir)))
          bundled-dir)
      dir)))

(setq package-user-dir (my-dir "d:/googledrive/emacs/share/emacs/site-lisp/elpa"))
(require 'package)
(package-initialize)

(set-frame-height nil 105)
(set-frame-width nil 538)
(set-frame-position (selected-frame) 5 6)
;; You seem not to want this, that's why you delete windows
(setq inhibit-startup-screen t)

;; *cg* position initial window */
(defun my-set-default-window-layout ()
  "set window pos and splits to default"
  (interactive)
  (delete-other-windows)
  ;; `setq' instead of `let'. We'll use them later
  (setq my-curbuf (current-buffer)
        my-right-window (split-window-right)
        my-middle-window (split-window-right)
        my-big-parrent-window (window-parent my-middle-window))
  (balance-windows my-big-parrent-window)
  (setq my-bottom-left-window (condition-case nil
                                  (split-window-below 66)
                                ;; Handle excessively small screens like mine.
                                (error (split-window-below)))
        my-middle-left-window (split-window-below)
        my-top-left-window (frame-first-window)
        my-little-parent-window (window-parent my-top-left-window))
  (select-window my-middle-window))

(defvar my-windows
  '(my-right-window
    my-middle-window
    my-bottom-left-window
    my-middle-left-window
    my-top-left-window))

;; Could be a macro but it's likely overkill.
;; Elisp is a lisp-2 so we can reuse the names
(dolist (win my-windows)
  (fset win (lambda (buffer alist)
              (window--display-buffer
               buffer (symbol-value win) 'reuse alist))))

;; This is where you define window behaviour It's more complicated than that,
;; but for your use case, just make each member of the alist of the form (REGEX
;; (FUNCTION)). The docs go into more detail but I don't think you'll need it
(setq display-buffer-alist
      '(("\\*shell\\*" (my-bottom-left-window))
        ("\\*compilation\\*" (my-bottom-left-window))))

(defun my-ediff-setup-function (buffer-A buffer-B buffer-C control-buffer)
  "Stick BUFFER-A BUFFER-B in `my-right-window' and `my-middle-window'.
If BUFFER-C is there, create a new temporary full-height window on the right and put it there.
But the control buffer in `my-top-left-window'."
  (when buffer-C
    (select-window my-right-window)
    ;; Don't use `let' we need it later
    (setq my-extra-ediff-window (split-window-right)
          ediff-window-C my-extra-ediff-window)
    ;; Make sure all 3 tall windows are equal
    (balance-windows my-big-parrent-window))
  (with-current-buffer control-buffer
      (setq ediff-window-A my-middle-window
            ediff-window-B my-right-window
            ;; Tweak this if you like, you didn't state your preference.
            ediff-control-window my-bottom-left-window))
  (select-window ediff-control-window))

(defun my-ediff-cleanup-function ()
  "Delete extra ediff window"
  (when (bound-and-true-p my-extra-ediff-window)
    (quit-window my-extra-ediff-window)))

;; Move this to a `use-package' statement leter
(setq ediff-window-setup-function #'my-ediff-setup-function)
(with-eval-after-load 'ediff (add-to-list 'ediff-quit-hook #'my-ediff-cleanup-function))

(defun my-ediff-setup-windows-plain (buffer-A buffer-B buffer-C control-buffer)
  (with-current-buffer control-buffer
    (setq ediff-multiframe nil))
  (if ediff-merge-job
      (my-ediff-setup-windows-plain-merge
       buffer-A buffer-B buffer-C control-buffer)
    (my-ediff-setup-windows-plain-compare
     buffer-A buffer-B buffer-C control-buffer)))

(defun my-ediff-setup-windows-plain-merge (buf-A buf-B buf-C control-buffer)
  ;; skip dedicated and unsplittable frames
  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 1)
	(with-Ancestor-p (with-current-buffer control-buffer
                           ediff-merge-with-ancestor-job))
	split-window-function
	merge-window-share merge-window-lines
	(buf-Ancestor (with-current-buffer control-buffer
                        ediff-ancestor-buffer))
	wind-A wind-B wind-C wind-Ancestor)
    (with-current-buffer control-buffer
      (setq merge-window-share ediff-merge-window-share
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)

    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1)
    (setq merge-window-lines
	  (max 2 (round (* (window-height) merge-window-share))))
    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))

    (split-window-vertically (max 2 (- (window-height) merge-window-lines)))
    (if (eq (selected-window) wind-A)
	(other-window 1))
    (setq wind-C (selected-window))
    (switch-to-buffer buf-C)

    (when (and ediff-show-ancestor with-Ancestor-p)
      (select-window wind-C)
      (funcall split-window-function)
      (when (eq (selected-window) wind-C)
        (other-window 1))
      (switch-to-buffer buf-Ancestor)
      (setq wind-Ancestor (selected-window)))

    (select-window wind-A)
    (funcall split-window-function)

    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))

    (with-current-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C
            ediff-window-Ancestor wind-Ancestor))

    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))

(defun my-ediff-setup-windows-plain-compare (buf-A buf-B buf-C control-buffer)
  ;; skip dedicated and unsplittable frames
  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 1)
	split-window-function wind-width-or-height
	three-way-comparison
	wind-A-start wind-B-start wind-A wind-B wind-C)
    (with-current-buffer control-buffer
      (setq wind-A-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'A ediff-narrow-bounds))
	    wind-B-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'B  ediff-narrow-bounds))
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function
	    three-way-comparison ediff-3way-comparison-job))
    ;; if in minibuffer go somewhere else
    (if (save-match-data
	  (string-match "\\*Minibuf-" (buffer-name (window-buffer))))
	(select-window (next-window nil 'ignore-minibuf)))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (split-window-vertically)
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)

    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1)
    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))
    (if three-way-comparison
	(setq wind-width-or-height
	      (/ (if (eq split-window-function #'split-window-vertically)
		     (window-height wind-A)
		   (window-width wind-A))
		 3)))

    (funcall split-window-function wind-width-or-height)

    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))

    (if three-way-comparison
	(progn
	  (funcall split-window-function) ; equally
	  (if (eq (selected-window) wind-B)
	      (other-window 1))
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))))

    (with-current-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C))

    ;; It is unlikely that we will want to implement 3way window comparison.
    ;; So, only buffers A and B are used here.
    (if ediff-windows-job
	(progn
	  (set-window-start wind-A wind-A-start)
	  (set-window-start wind-B wind-B-start)))

    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))

(defun my-send-buffers-to-window (regex window &optional now visible)
  "Send buffers matching REGEX to WINDOW.
If NOW is t, cycle through all buffers and sort them.
If VISIBLE is t, only cycle through visible buffers"
  ())


(add-to-list 'load-path (my-dir "d:/dev/code/emacs/"))

(load (my-dir "d:/dev/code/emacs/.emacs_shared"))
(my-set-default-window-layout)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-image-file-mode t nil (image-file))
 '(backup-directory-alist '(("." . "c:/emacs-backup/")))
 '(c++-font-lock-extra-types
   '("\\sw+_t" "\\([iof]\\|str\\)+stream\\(buf\\)?" "ios" "string" "rope" "list" "slist" "deque" "vector" "bit_vector" "set" "multiset" "map" "multimap" "hash\\(_\\(m\\(ap\\|ulti\\(map\\|set\\)\\)\\|set\\)\\)?" "stack" "queue" "priority_queue" "type_info" "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator" "reference" "const_reference" "ULONG" "int32" "UBYTE" "WORD" "UWORD" "vec3" "vec4" "mat3" "uint\\d+" "int\\d+" "ConVar" "ITexture" "IMaterial" "Vector" "float[234]" "fltx4" "float[234]x[234]"))
 '(c-noise-macro-names '("constexpr"))
 '(column-number-mode t)
 '(compilation-scroll-output 'first-error)
 '(cppcm-build-dirname "d:/dev/code")
 '(explicit-shell-file-name "C:\\Program Files\\JPSoft\\TCCLE14x64\\tcc.exe")
 '(fringe-mode 0 nil (fringe))
 '(image-file-name-extensions
   '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm"))
 '(kept-old-versions 10)
 '(large-file-warning-threshold 40000000)
 '(package-selected-packages
   '(svg-tag-mode quelpa async psession async-await treemacs-perspective treemacs-all-the-icons git treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs adaptive-wrap latex-math-preview auctex cpputils-cmake modern-cpp-font-lock ## sr-speedbar))
 '(tab-width 4)
 '(tags-case-fold-search nil)
 '(tags-revert-without-query t)
 '(text-mode-hook
   '((lambda nil
       (local-set-key "\211" 'ispell-complete-word))))
 '(tool-bar-mode nil)
 '(w32-list-proportional-fonts t t)
 '(which-function-mode t nil (which-func))
 '(window-divider-default-right-width 4)
 '(window-divider-mode t))




;;(my-set-default-window-layout)
;;(treemacs)




;;(set-window-buffer-in-frame 0 0 (shell ) )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Tahoma" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 '(font-lock-bs-keywords-face ((t (:slant italic :height 0.85))))
 '(font-lock-comment-face ((t (:foreground "blue" :slant italic :family "Times New Roman"))))
 '(font-lock-commented-out-code-face ((t (:height 0.75))))
 '(font-lock-constant-face ((t (:foreground "navy" :underline t))))
 '(font-lock-function-name-face ((t (:background "seashell" :foreground "black" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "HotPink3"))))
 '(font-lock-level0-comment-face ((t (:foreground "medium blue" :family "Times New Roman"))))
 '(font-lock-level2-comment-face ((t (:background "old lace" :foreground "light slate gray" :height 1 :family "Times New Roman"))))
 '(font-lock-level3-comment-face ((t (:foreground "slate gray" :family "Times New Roman"))))
 '(font-lock-level4-comment-face ((t (:background "old lace" :foreground "black" :height 1.2 :family "Times New Roman"))))
 '(font-lock-level5-comment-face ((t (:weight bold :height 1.25))))
 '(font-lock-post-declaration-doc-face ((t (:background "ghost white" :foreground "navy" :family "Times New Roman"))))
 '(font-lock-toplevel-comment-face ((t (:height 1.5 :family "Times New Roman"))))
 '(font-lock-variable-name-face ((t nil)))
 '(hl-line ((t (:background "alice blue"))))
 '(internal-border ((t (:background "gray"))))
 '(minibuffer-prompt ((t (:foreground "darkgreen"))))
 '(mode-line ((t (:background "royal blue" :foreground "white" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gainsboro" :foreground "grey20" :box (:line-width -1 :color "blue") :weight light))))
 '(svg-tag-default-face ((t (:background "orange" :foreground "white" :box (:line-width 1 :color "orange") :weight normal :height 120))))
 '(tag-default-face ((t (:box (:line-width 1 :color "lemon chiffon") :weight light :height 120 :family "Roboto Mono"))))
 '(tag-note-face ((t nil)))
 '(window-divider ((t (:foreground "gainsboro"))))
 '(window-divider-first-pixel ((t (:foreground "sky blue"))))
 '(window-divider-last-pixel ((t (:foreground "blue")))))
