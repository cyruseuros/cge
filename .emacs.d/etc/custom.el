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
