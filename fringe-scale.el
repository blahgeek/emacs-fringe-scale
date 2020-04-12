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

;;;###autoload
(defun fringe-scale-setup ()
  "Setup fringe scale.
Must be called before other packages calling `define-fringe-bitmap'."
  (interactive)
  (advice-add 'define-fringe-bitmap :around #'fringe-scale--define-fringe-bitmap-advice))


(provide 'fringe-scale)
;;; fringe-scale.el ends here
