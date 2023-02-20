;;; doc-scrap.el --- "Read and edit documents"      -*- lexical-binding: t; -*-

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

(require 'epc)
(load-file "/home/dalanicolai/git/doc-tools-pymupdf/doc-pymupdf-client.el")
;; (require 'scrap-mode)
(load-file "scrap-mode.el")

(defun doc-scrap-kill-server ()
  (epc:stop-epc doc-pymupdf-server))

(defvar-local doc-scrap-doc nil)

;; by including one more row before/after, images are already displayed before
;; resulting in smoother scrolling
(defsubst doc-scrap-visible-overlays ()
  (overlays-in (- (window-start) (* 2 scrap-columns) 2)
               (+ (window-end) (* 2 scrap-columns))))

(define-derived-mode doc-scrap-mode special-mode "DocScrap"
  (let ((default-directory "/home/dalanicolai/git/doc-tools-pymupdf/"))
    (setq doc-pymupdf-server (epc:start-epc "python" '("doc-pymupdf-server.py"))))
  (doc-pymupdf-init buffer-file-name)
  (add-hook 'kill-buffer-hook #'doc-scrap-kill-server nil t)

  ;; (doc-mupdf-create-pages doc-scrap-overlay-width)

  ;; (doc-scrap-minor-mode)

  (setq-local doc-scrap-internal-page-sizes (doc-pymupdf-page-sizes)
              doc-scrap-last-page (length doc-scrap-internal-page-sizes)
              ;; doc-scrap-structured-contents (doc-poppler-structured-contents nil nil t)

              ;; doc-scrap-display-page-function #'doc-backend-djvu-display-page
              doc-scrap-image-type 'png
              ;; doc-scrap-image-data-function #'mupdf-get-image-data
              doc-scrap-image-data-function #'doc-pymupdf-page-base64-image-data
              ;; doc-scrap-image-data-function #'doc-backend-pymupdf-image-data

              ;; imenu-create-index-function #'doc-backend-mupdf--imenu-create-index

              ;; imenu-create-index-function #'doc-backend-pymupdf--imenu-create-index
              ;; imenu-default-goto-function (lambda (_name position &rest _rest)
              ;;                               ;; NOTE VERY WEIRD, the first
              ;;                               ;; result is a number, while the
              ;;                               ;; other results are markers
              ;;                               (doc-scrap-goto-page (if (markerp position)
              ;;                                                    (marker-position position)
              ;;                                                  position)))

              doc-scrap-info-function #'doc-pymupdf-info

              scrap-next-function #'doc-scrap-next))

;; (setq magic-mode-alist (remove '("%PDF" . pdf-view-mode) magic-mode-alist))
;; (add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-backend-pymupdf-mode))

(defun doc-scrap-overlay-base-width (columns)
  (let ((win-width (- (nth 2 (window-inside-pixel-edges))
                      (nth 0 (window-inside-pixel-edges)))))
    (/ (- win-width (* (1- columns) (frame-char-width)))
       columns)))

(defun doc-scroll-overlay-base-height (base-width)
  (let* ((widest-page-w (apply #'max (mapcar #'car doc-scrap-internal-page-sizes)))
         (ratio (/ base-width widest-page-w)))
    (* ratio (apply #'max (mapcar #'cdr doc-scrap-internal-page-sizes)))))

(defun doc-scrap-display-page (overlay)
  (deferred:$
   (epc:call-deferred doc-pymupdf-server 'renderpage_data (list (overlay-get overlay 'n)
                                                                (car (overlay-get overlay 'size))))
   (deferred:nextc it
                   (lambda (x)
                     (overlay-put overlay
                                  'display (create-image (base64-decode-string x) 'png t))))))

(defun doc-scrap-undisplay-page (overlay)
  (let ((size (overlay-get overlay 'size)))
    (overlay-put overlay 'display `(space . (:width (,(car size)) :height (,(cdr size)))))))

(defun doc-find-file (file-name)
  (interactive
   (list (read-file-name "Select document: " nil nil t nil
                         (lambda (name) (or (file-directory-p name)
                                            (string= (file-name-extension name) "pdf"))))))
  (pop-to-buffer-same-window (get-buffer-create file-name))
  (setq buffer-file-name file-name)
  (doc-scrap-mode)

  (let ((inhibit-read-only t)) ; parent-mode special-mode is read-only
    (scrap-create-overlays doc-scrap-last-page 4 nil nil
                           'face `(:background "gray")))

  (let* ((w (doc-scrap-overlay-base-width scrap-columns))
         (h (doc-scroll-overlay-base-height w)))
    (dolist (o (doc-scrap-visible-overlays))
      (overlay-put o 'display `(space . (:width (,w) :height (,h))))
      (overlay-put o 'size (cons w h)))
    (sit-for 0.01) ; wait for the overlay display property to take effect
    (dolist (o (doc-scrap-visible-overlays))
      (doc-scrap-display-page o))))

(defun doc-scrap-next (n &optional previous)
  (interactive "p")
  (let* ((current (1- (scrap-get-overlay-n)))
         (next (nth (+ current (if previous (- n) n)) scrap-overlays))
         (before (doc-scrap-visible-overlays))
          after)
    (goto-char (overlay-start next))
    (sit-for 0.002) ; somehow overlays need time to update/redisplay
    (setq after (doc-scrap-visible-overlays))
    (dolist (o (seq-difference before after))
      (doc-scrap-undisplay-page o))
    (dolist (o (seq-difference after before))
      (doc-scrap-display-page o))))

;; (defun doc-scrap-scroll-forward (&optional screen)
;;   (interactive)
;;   (let ((new-vscroll (+ (window-vscroll nil t) 10)))
;;     (cond ((> new-vscroll (cdr (doc-scrap-current-size)))
;;            (forward-line)
;;            (doc-scrap-set-window-fscroll 0) ;or set to vertical margin
;;            (doc-scrap-update)
;;            (run-hooks 'doc-scrap-after-change-page-hook))
;;           ((> (+ new-vscroll (window-text-height nil t)) (cdr (doc-scrap-current-size)))
;;            (cond ((and (= (doc-scrap-columns) 1) (= (doc-scrap-page-at-point) doc-scrap-last-page))
;;                   (message "End of buffer"))
;;                  (t
;;                   (doc-scrap-set-window-fscroll new-vscroll)
;;                   (doc-scrap-update))))
;;           (t (doc-scrap-set-window-fscroll new-vscroll)))))

;; (define-minor-mode doc-scrap-mode "Display documents."
;;   :lighter "DS"
;;   :keymap `((,(kbd "C-n") . doc-scrap-next)
;;             (,(kbd "C-p") . doc-scrap-previous)
;;             (,(kbd "C-f") . doc-scrap-next-line)
;;             (,(kbd "C-b") . doc-scrap-previous-line)))

(provide 'doc-scrap)
;;; doc-scrap.el ends here
