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
(defvar scrap-incompatible-modes '(global-hl-line-mode))

(defvar-local scrap-overlays nil)
(defvar-local scrap-columns nil)

(defun scrap-get-overlay-n ()
  (overlay-get (car (overlays-at (point))) 'n))

(defun doc-find-file (file-name)
  (interactive
   (list (read-file-name "Select document: " nil nil t nil
                         (lambda (name) (or (file-directory-p name)
                                            (string= (file-name-extension name) "pdf"))))))
  (pop-to-buffer (get-buffer-create file-name))
  (setq buffer-file-name file-name)
  (scrap-create-overlays 10 '(400 . 560) 2 nil nil
                         'face `(:background "gray")))
(defun scrap-images (dir)
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
    (setq scrap-overlays (nreverse overlays))))

(defun scrap-next (n &optional previous)
  (interactive "p")
  (let* ((current (1- (scrap-get-overlay-n)))
         (next (nth (+ current (if previous (- n) n)) scrap-overlays)))
    (goto-char (overlay-start next))))

(defun scrap-previous (n &optional previous)
  (interactive "p")
  (scrap-next n t))

(defun scrap-next-line (n &optional previous)
  (interactive "p")
  (scrap-next (* n scrap-columns)))

(defun scrap-previous-line (n &optional previous)
  (interactive "p")
  (scrap-next (* n scrap-columns) t))


(provide 'image-scroll-mode)
;;; image-scroll-mode.el ends here
