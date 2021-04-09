
(defvar font-lock-level5-comment-face
  'font-lock-level5-comment-face
  "Face name to use for triple slash comments at indent level 0"
  )

(defface font-lock-level5-comment-face
  '((t (:foreground "OrangeRed1")))
  "Face to use for triple slash comments at indent level 0"
  :group 'font-lock-faces)

(defvar font-lock-level4-comment-face
  'font-lock-level4-comment-face
  "Face name to use for triple slash comments at indent level 0"
  )

(defface font-lock-level4-comment-face
  '((t (:foreground "OrangeRed1")))
  "Face to use for triple slash comments at indent level 0"
  :group 'font-lock-faces)

(defvar font-lock-level3-comment-face
  'font-lock-level3-comment-face
  "Face name to use for double slash comments at indent level 0"
  )

(defface font-lock-level3-comment-face
  '((t (:foreground "OrangeRed1")))
  "Face to use for double slash comments at indent level 0"
  :group 'font-lock-faces)

(defvar font-lock-level2-comment-face
  'font-lock-level2-comment-face
  "Face name to use for triple slash full line comments at indent level >0"
  )

(defface font-lock-level2-comment-face
  '((t (:foreground "OrangeRed1")))
  "Face to use for triple slash full line comments at indent level >00"
  :group 'font-lock-faces)

(defvar font-lock-post-declaration-doc-face
  'font-lock-post-declaration-doc-face
  "Face name to use for //< full line comments at indent level >0"
  )

(defface font-lock-post-declaration-doc-face
  '((t (:foreground "OrangeRed1")))
  "Face to use for //< full line comments at indent level >00"
  :group 'font-lock-faces)

(defvar font-lock-level0-comment-face
  'font-lock-level0-comment-face
  "Face name to use for double slash non full-line comments"
  )

(defface font-lock-level0-comment-face
  '((t (:foreground "OrangeRed1")))
  "Face  to use for double slash non full-line comments"
  :group 'font-lock-faces)

(defvar font-lock-bs-keywords-face
  'font-lock-bs-keywords-face
  "Face name to use for de-emphasized keywords"
  )

(defface font-lock-bs-keywords-face
  '((t (:foreground "OrangeRed1")))
  "Face name to use for de-emphasized keywords"
  :group 'font-lock-faces)

(defvar font-lock-commented-out-code-face
  'font-lock-commented-out-code-face
  "Face name to use for commented out code"
  )
(defface font-lock-commented-out-code-face
  '((t (:foreground "OrangeRed1")))
  "Face  to use for commented out code"
  :group 'font-lock-faces)

(defface font-lock-solitary-brace-face
  '((t (:foreground "OrangeRed1")))
  "Face  to use for solitary brace lines"
  :group 'font-lock-faces)

(add-to-list 'font-lock-extra-managed-props 'display)

(defun cpp-prettify()
  "make code look pretty"

  ;; let symbol subsitition happen in comments
  (setq prettify-symbols-compose-predicate
        (defun my-prettify-symbols-default-compose-p (start end _match)
          "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
          (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                                   '(?w ?_) '(?. ?\\)))
                 (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                                   '(?w ?_) '(?. ?\\))))
            (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
                     (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
                     (nth 3 (syntax-ppss)))))))

  ;; add comment marker detection
  (font-lock-add-keywords nil
			  '(
			    ;;( "[;0-9a-zA-Z:]\\(\\s-+!!\\)" 1 `(face font-lock-level5-comment-face (display :align-to 60 ) ) t )
			    ("^\\(///\\S-.*\\)" 1 font-lock-level5-comment-face t )
			    ("^\\(///\\s-.*\\)" 1 font-lock-level4-comment-face t )
			    ("^\\(///$\\)" 1 font-lock-level4-comment-face t )
			    ("^\\(//\\s-.*\\)" 1 font-lock-level3-comment-face t )
			    ("^\\s-+\\(//<.*\\)" 1 font-lock-post-declaration-doc-face t )
			    ("^\\s-+\\(///<.*\\)" 1 font-lock-post-declaration-doc-face t )
			    ("^\\s-+\\(///.*\\)" 1 font-lock-level2-comment-face t )
			    ("^\\s-+\\(//\\s-.*\\)" 1 font-lock-level0-comment-face t )
			    ("^\\s-+\\(//;.*\\)" 1 font-lock-commented-out-code-face t )
			    ("^\\s-*\\([{}]\\);*\\s-*$" 1 font-lock-commented-out-code-face t )

                            ;; trailing comments
                            ("^ \\{0\\}[^ ].*[^ ]\\( +\\)//" 1 '(face nil display (space :align-to (250))))
                            ("^ \\{4\\}[^ ].*[^ ]\\( +\\)//" 1 '(face nil display (space :align-to (300))))
                            ("^ \\{8\\}[^ ].*[^ ]\\( +\\)//" 1 '(face nil display (space :align-to (350))))

                            ;; std::abs(x) == |x|
			    ("std::abs(" (0 '(face nil display "|" ))
                             (")" nil nil (0 '(face nil display "|" ))))
			    )
			  1
			  )

  (font-lock-add-keywords nil
                          '(
			    ("\\(\\.inverse()\\)" 1 '(face nil display "⁻¹" ) )
                            ;;							(" \\(=\\) " 1 '(face nil display "←" ) )
                            ;;							("^\\s-*\\S-.*\\(\\s-\\)//" 1 '(face nil display `(space :align-to 80 ) ) )
			    )
			  1
			  )

  (font-lock-add-keywords nil
                          '(
			    ("\\(delta_\\)" 1 '(face nil display "Δ" ) )
			    (" \\(ST\\)\\s-*(" 1 '(face nil display "∋" ) )
			    )
			  1
			  )

  (font-lock-add-keywords nil
			  '(
			    ("\\<\\(const\\)\\>" 1 font-lock-bs-keywords-face t )
			    ("\\<\\(inline\\)\\>" 1 font-lock-bs-keywords-face t )
			    ("\\<\\(INLINE\\)\\>" 1 font-lock-bs-keywords-face t )
			    ("\\<\\(void\\)\\>" 1 font-lock-bs-keywords-face t )
			    ("\\<\\(template\\)\\>" 1 font-lock-bs-keywords-face t )
			    ("\\<\\(virtual\\)\\>" 1 font-lock-bs-keywords-face t )
			    ("\\<\\(override\\)\\>" 1 font-lock-bs-keywords-face t )
			    )
			  1
			  )
  (setq prettify-symbols-alist
	'(
	  ("->" 8594)
	  ("==" 8801)
	  ("//<" 9615)
	  ("///+" 8239)
	  ("/// " 8239)
	  ("///" 8239)
	  (">=" . 8805)
	  ("<=" . 8804)
	  ("||" . 8214)
	  ("LAMBDA" . 955 )
	  ("!=" . 8800)
	  ("flAlpha" . 120514 )
	  ("flTheta" . 952 )
	  ("flBeta" . 120515)
	  ( "FLT_MAX" . 8734 )
	  ) )

  (prettify-symbols-mode 1 )
  ;; we want graphics in our comments
  (turn-on-iimage-mode)
  (iimage-mode-buffer t ))

(provide 'prettycpp)


;; todo:
;;   Highlight "operatorx" decls like function names
;;   make a list of "bullshit" keywords to de emphasize
;; make a list of real bullshit words to hide in doc or RO mode. _NODDISCARD, INLINE..
;; hige prepc directives in ro mode
;; //- take which removes anything on it and anything indented more. add to the end or head of a class comment.
;; autohiding bodies
;; standalong tempatle lines are awful


;; implement doxy style comment mathings. Like ///< for an indented trailing block comment after a decl
(with-temp-buffer
  (insert "x1")
  (put-text-property 2 3 'display '(raise 0.5))
  (message "%s" (buffer-string))
  (sit-for 3))
