;; ---------------------------------------------------------------------
;; Tag minor mode
;; Copyright (C) 2020 Nicolas .P Rougier
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

(require 's)
(require 'svg)
(require 'cl-lib)

(defface tag-default-face
  '((t :foreground "white" :background "orange" :box "orange"
       :family "Roboto Mono" :weight light :height 120))
  "Default face for tag" :group 'tag-mode)

(defun make-tag (text &optional face inner-padding outer-padding radius)
  (let* ((face       (or face 'tag-default-face))
         (foreground (face-attribute face :foreground))
         (background (face-attribute face :background))
         (border     (face-attribute face :box))
         (family     (face-attribute face :family))
         (weight     (face-attribute face :weight))
         (size       (/ (face-attribute face :height) 10))

         (tag-char-width  (window-font-width nil face))
         (tag-char-height (window-font-height nil face))
         (txt-char-width  (window-font-width))
         (txt-char-height (window-font-height))
         (inner-padding   (or inner-padding 1))
         (outer-padding   (or outer-padding 0))

         (text (s-trim text))
         (tag-width (* (+ (length text) inner-padding) txt-char-width))
         (tag-height (* txt-char-height 0.9))
         
         (svg-width (+ tag-width (* outer-padding txt-char-width)))
         (svg-height tag-height)

         (tag-x (/ (- svg-width tag-width) 2))
         (text-x (+ tag-x (/ (- tag-width (* (length text) tag-char-width)) 2)))
         (text-y (- tag-char-height (- txt-char-height tag-char-height)))
         
         (radius  (or radius 3))
         (svg (svg-create svg-width svg-height)))
         
    (svg-rectangle svg tag-x 0 tag-width tag-height
                   :fill        border
                   :rx          radius)
    (svg-rectangle svg (+ tag-x 0.5) 0.5 (- tag-width 1.0) (- tag-height 1.0)
                   :fill        background
                   :rx          (- radius 0.5))
    (svg-text      svg text 
                   :font-family family
                   :font-weight weight
                   :font-size   size
                   :fill        foreground
                   :x           text-x
                   :y           text-y)
    (svg-image svg :ascent 'center)))


(defface tag-note-face
  '((t :foreground "black" :background "yellow" :box "black"
       :family "Roboto Mono" :weight light :height 120))
  "Face for note tag" :group nil)

(defface tag-key-face
  '((t :foreground "#333333" :background "#f0f0f0" :box "#333333"
       :family "Roboto Mono" :weight light :height 120))
  "Face for key tag" :group nil)

(setq tag-todo (make-tag "TODO" nil 1 1 2))
(setq tag-note (make-tag "NOTE" 'tag-note-face 1 1 2))
(defun tag-key (text)
  (make-tag (substring text 1 -1) 'tag-key-face 1 1 2))

;;(define-minor-mode tag-mode
;;  "Minor mode for graphical tag as rounded box."
;;  :lighter " tag"
;;  )

(defgroup tag nil
  "Graphical tags"
  :group 'faces)

(defun tag-mode-enter ()
  ;; (make-local-variable 'font-lock-extra-managed-props) 
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords nil
        '(("\\(\:TODO\:\\)" 1
           `(face nil display ,tag-todo))
          ("\\(\:NOTE\:\\)" 1
           `(face nil display ,tag-note))
          ("\\(=[0-9a-zA-Z- ]+?=\\)" 1
           `(face nil display ,(tag-key (match-string 0))))))
  
  (message "Tag mode enter"))

(defun tag-mode-exit ()
  (font-lock-remove-keywords nil
        '(("\\(\:TODO\:\\)" 1 `(face nil display ,tag-todo))
          ("\\(\:NOTE\:\\)" 1 `(face nil display ,tag-note))
          ("\\(=[0-9a-zA-Z- ]+?=\\)" 1
                              `(face nil display ,(tag-key (match-string 0))))))
  (message "Tag mode exit"))


(define-minor-mode tag-mode
  "Minor mode for graphical tag as rounded box."
  :group 'tag-mode
  (if tag-mode (tag-mode-enter) (tag-mode-exit))
  (font-lock-flush))


;; A tag function using SVG to display a rounded box with outer and inner
;; padding and a controllable box radius. The resulting SVG is perfectly
;; aligned with regular text such that a =TAG= can be inserted and edited
;; anywhere in the text thanks to font-lock and the display property.

;;|:TODO:| Make a minor mode
;;|:NOTE:| Don't know how to do it, help needed…
;;|______| Perfect alignment with regular text
;;
;;  Save ................. =C-x=+=C-s=  Help ............... =C-h=
;;  Save as .............. =C-x=+=C-w=  Cancel ............. =C-g=
;;  Open a new file ...... =C-x=+=C-f=  Undo ............... =C-z=
;;  Open recent .......... =C-x=+=C-r=  Close buffer ....... =C-x=+=k=
;;  Browse directory ......=C-x=+=d=    Quit ............... =C-x=+=C-c=

;; ------------------------------------------------------------------------
;; :NOTE: Sections can be folded or unfolded. If you think a section has
;;        disappeared, it's probably because it is folded. To unfold it,
;;        place the cursor on the section title and press the =tab= key.
;; ------------------------------------------------------------------------
(provide 'tag)

