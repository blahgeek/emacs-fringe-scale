;;; fringe-scale.el --- Scale fringe bitmap for HiDPI screens  -*- lexical-binding: t; -*-

;; Author:  <i@blahgeek.com>
;; Keywords: extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple package to scale fringe bitmap for HiDPI screens.
;;
;; This package works by adding advice to `define-fringe-bitmap' to
;; scale bitmaps to target width.
;;
;; Simply call `fringe-scale-setup' before any other packages which may
;; define custom fringes.

;;; Code:

(defgroup fringe-scale nil
  "Scale fringe bitmap"
  :group 'tools)

(defcustom fringe-scale-width 16
  "Target width when scaling fringe bitmaps."
  :type 'number
  :group 'fringe-scale)


(defun fringe-scale--scale-width (x orig-width new-width)
  "Scale width of fringe bitmap."
  (let ((res 0) (i 0))
    (while (< i new-width)
      (let* ((j (floor (* orig-width (/ (float i) new-width))))
             (bit (logand 1 (lsh x (- j)))))
        (setq res (logior res (lsh bit i)))
        )
      (setq i (1+ i)))
    res))

(defun fringe-scale--scale-height (v orig-height new-height)
  "Scale height of fringe bitmap."
  (let ((res (make-vector new-height nil)) (i 0))
    (while (< i new-height)
      (let* ((j (floor (* orig-height (/ (float i) new-height))))
             (val (elt v j)))
        (aset res i val))
      (setq i (1+ i)))
    res))

(defun fringe-scale--define-fringe-bitmap-advice (orig-func &rest r)
  "Advice for define-fringe-bitmap, scale the bitmap if required."
  (let* ((bitmap (nth 0 r))
         (bits (nth 1 r))
         (height (or (nth 2 r) (length bits)))
         (width (or (nth 3 r) 8))
         (align (or (nth 4 r) 'center)))
    (when (and (< width fringe-scale-width))
      (message "Scaling fringe bitmap %s: width %d to %d" bitmap width fringe-scale-width)
      (let* ((new-width fringe-scale-width)
             (new-height (floor (* height (/ (float new-width) width))))
             (bits-w-scaled (mapcar (lambda (x) (fringe-scale--scale-width x width new-width)) bits))
             (bits-h-scaled (fringe-scale--scale-height bits-w-scaled height new-height)))
        (setq bits bits-h-scaled)
        (setq height new-height)
        (setq width new-width)
        ))
    (funcall orig-func bitmap bits height width align)
    )
  )

(defun fringe-scale--redefine-builtin-bitmaps ()
  "redefine and scale the builtin bitmaps, which was defined in c code"
  (let ((question-mark [#x3c #x7e #xc3 #xc3 #x0c #x18 #x18 #x00 #x18 #x18])
      (exclamation-mark [#x18 #x18 #x18 #x18 #x18 #x18 #x18 #x00 #x18 #x18])
      (left-arraw [#x18 #x30 #x60 #xfc #xfc #x60 #x30 #x18])
      (right-arrow [#x18 #x0c #x06 #x3f #x3f #x06 #x0c #x18])
      (up-arrow [#x18 #x3c #x7e #xff #x18 #x18 #x18 #x18])
      (down-arrow [#x18 #x18 #x18 #x18 #xff #x7e #x3c #x18])
      (left-curly-arrow [#x3c #x7c #xc0 #xe4 #xfc #x7c #x3c #x7c])
      (right-curly-arrow [#x3c #x3e #x03 #x27 #x3f #x3e #x3c #x3e])
      (left-triangle [#x03 #x0f #x1f #x3f #x3f #x1f #x0f #x03])
      (right-triangle [#xc0 #xf0 #xf8 #xfc #xfc #xf8 #xf0 #xc0])
      (top-left-angle [#xfc #xfc #xc0 #xc0 #xc0 #xc0 #xc0 #x00])
      (top-right-angle [#x3f #x3f #x03 #x03 #x03 #x03 #x03 #x00])
      (bottom-left-angle [#x00 #xc0 #xc0 #xc0 #xc0 #xc0 #xfc #xfc])
      (bottom-right-angle [#x00 #x03 #x03 #x03 #x03 #x03 #x3f #x3f])
      (left-bracket [#xfc #xfc #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xfc #xfc])
      (right-bracket [#x3f #x3f #x03 #x03 #x03 #x03 #x03 #x03 #x3f #x3f])
      (filled-rectangle [#xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe])
      (hollow-rectangle [#xfe #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #xfe])
      (hollow-square [#x7e #x42 #x42 #x42 #x42 #x7e])
      (filled-square [#x7e #x7e #x7e #x7e #x7e #x7e])
      (vertical-bar [#xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0])
      (horizontal-bar [#xfe #xfe]))
  (define-fringe-bitmap 'question-mark question-mark)
  (define-fringe-bitmap 'exclamation-mark exclamation-mark)
  (define-fringe-bitmap 'left-arraw left-arraw)
  (define-fringe-bitmap 'right-arrow right-arrow)
  (define-fringe-bitmap 'up-arrow up-arrow)
  (define-fringe-bitmap 'down-arrow down-arrow)
  (define-fringe-bitmap 'left-curly-arrow left-curly-arrow)
  (define-fringe-bitmap 'right-curly-arrow right-curly-arrow)
  (define-fringe-bitmap 'left-triangle left-triangle)
  (define-fringe-bitmap 'right-triangle right-triangle)
  (define-fringe-bitmap 'top-left-angle top-left-angle)
  (define-fringe-bitmap 'top-right-angle top-right-angle)
  (define-fringe-bitmap 'bottom-left-angle bottom-left-angle)
  (define-fringe-bitmap 'bottom-right-angle bottom-right-angle)
  (define-fringe-bitmap 'left-bracket left-bracket)
  (define-fringe-bitmap 'right-bracket right-bracket)
  (define-fringe-bitmap 'filled-rectangle filled-rectangle)
  (define-fringe-bitmap 'hollow-rectangle hollow-rectangle)
  (define-fringe-bitmap 'hollow-square hollow-square)
  (define-fringe-bitmap 'filled-square filled-square)
  (define-fringe-bitmap 'vertical-bar vertical-bar)
  (define-fringe-bitmap 'horizontal-bar horizontal-bar)))

;;;###autoload
(defun fringe-scale-setup ()
  "Setup fringe scale.
Must be called before other packages calling `define-fringe-bitmap'."
  (interactive)
  (advice-add 'define-fringe-bitmap :around #'fringe-scale--define-fringe-bitmap-advice)
  (fringe-scale--redefine-builtin-bitmaps))


(provide 'fringe-scale)
;;; fringe-scale.el ends here
