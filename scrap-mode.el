;;; image-scroll-mode.el --- Image and document display engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; Keywords: tools

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

;;

;;; Code:
(defvar scrap-incompatible-modes '(visual-line-mode
                                   global-hl-line-mode))

(defvar-local scrap-overlays nil)
(defvar-local scrap-columns nil)
(defvar-local scrap-next-function #'scrap-next)

(defsubst scrap-current-overlay ()
  (car (overlays-at (point))))

(defsubst scrap-get-overlay-n ()
  (overlay-get (scrap-current-overlay) 'n))

(defun scrap-dir-images (dir)
  (interactive
   (list (read-file-name "Select document: " nil nil t nil #'file-directory-p)))
  (pop-to-buffer (get-buffer-create dir))
  (setq buffer-file-name dir)
  (let ((images (seq-filter #'image-supported-file-p (directory-files dir)))
	(max-w 150)
	;; (max-h 200)
	)
    (scrap-create-overlays (length images) 5)
    (seq-do-indexed (lambda (im n)
		      (overlay-put (nth n scrap-overlays)
				   'display (create-image (concat (file-name-as-directory dir) im) nil nil
							  :max-width max-w)))
		    images)))


(defun scrap-create-overlays (number
                              ;; size
                              &optional columns no-hspace no-vspace
                              &rest overlay-props)
  (dolist (m scrap-incompatible-modes)
    (funcall m -1))
  (toggle-truncate-lines 1) ; also disables visual-mode
  (setq scrap-columns columns)
  (let (overlays)
    (dotimes (i number)
      (let* ((n (1+ i))
             (o (make-overlay
                 ;; (prog1 (point) (insert (make-string doc-scroll-line-length (string-to-char " "))))
                 ;; (point))))
                 (point)
                 (progn (insert " ") (point))
                 (unless no-hspace (insert " "))))
             ;; (w (car size))
             ;; (h (cdr size))
	     )
        (when (and (= (% n (or columns 1)) 0)
                   (not (= n number)))
          (insert "\n")
          (unless no-vspace (insert "\n")))
        (overlay-put o 'n n)
        ;; (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (dotimes (j (/ (length overlay-props) 2))
          (let ((m (* j 2)))
            (overlay-put o (nth m overlay-props) (nth (+ m 1) overlay-props))))
        (push o overlays)))
    (goto-char (point-min))
    (setq scrap-overlays (nreverse overlays))
    (scrap-mode)))

(defun scrap-next (n &optional previous)
  (let* ((current (1- (scrap-get-overlay-n)))
         (next (nth (+ current (if previous (- n) n)) scrap-overlays)))
    (goto-char (overlay-start next))))

(defun scrap-scroll-next (n)
  (interactive "p")
  (funcall scrap-next-function n))

(defun scrap-scroll-previous (n)
  (interactive "p")
  (funcall scrap-next-function n t))

(defun scrap-scroll-next-line (n)
  (interactive "p")
  (funcall scrap-next-function (* n scrap-columns)))

(defun scrap-scroll-previous-line (n)
  (interactive "p")
  (funcall scrap-next-function (* n scrap-columns) t))

(define-minor-mode scrap-mode "Display images in a grid."
  :lighter "Scrap"
  :keymap `((,(kbd "C-n") . scrap-scroll-next)
            (,(kbd "C-p") . scrap-scroll-previous)
            (,(kbd "C-f") . scrap-scroll-next-line)
            (,(kbd "C-b") . scrap-scroll-previous-line)))

(provide 'image-scroll-mode)
;;; image-scroll-mode.el ends here
