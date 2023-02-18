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
(defun scrap-get-overlay-n ()
  (overlay-get (car (overlays-at (point))) 'n))

(defun doc-find-file (file-name)
  (interactive
   (list (read-file-name "Select document: " nil nil t nil
                         (lambda (name) (or (file-directory-p name)
                                            (string= (file-name-extension name) "pdf"))))))
  (pop-to-buffer (get-buffer-create file-name))
  (setq buffer-file-name file-name)
  (scrap-create-overlays 10 '(100 . 140) 2 nil nil
                         'face `(:background "gray")))

(defvar-local scrap-overlays nil)

(defun scrap-create-overlays (number
                              size
                              &optional columns no-hspace no-vspace
                              &rest overlay-props)
  (let (overlays)
    (dotimes (i number)
      (let* ((n (1+ i))
             (o (make-overlay
                 ;; (prog1 (point) (insert (make-string doc-scroll-line-length (string-to-char " "))))
                 ;; (point))))
                 (point)
                 (progn (insert " ") (point))
                 (unless no-hspace (insert " "))))
             (w (car size))
             (h (cdr size)))
        (when (and (= (% n (or columns 1)) 0)
                   (not (= n number)))
          (insert "\n")
          (unless no-vspace (insert "\n")))
        (overlay-put o 'n n)
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (dotimes (j (/ (length overlay-props) 2))
          (let ((m (* j 2)))
            (overlay-put o (nth m overlay-props) (nth (+ m 1) overlay-props))))
        (push o overlays)))
    (setq scrap-overlays (nreverse overlays))))

(defun scrap-next (n &optional previous)
  (interactive "p")
  (let* ((current (1- (scrap-get-overlay-n)))
         (next (nth (+ current (if previous (- n) n)) scrap-overlays)))
    (goto-char (overlay-start next))))


(provide 'image-scroll-mode)
;;; image-scroll-mode.el ends here
