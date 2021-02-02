;;; annotator.el --- NLP Text Annotating Tool        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <jingxiaobing@gmail.com>
;; Keywords: tools, convenience

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

;; NLP Text Annotating Tool

;;; Code:

(defface annotation-edge-face
  '((t :inherit font-lock-comment-face
       :height 0.7))
  "Annotation edge face"
  :group 'annotator-mode)

;; defface只声明，不改变已有值，调试用set-face-attribute
;;(set-face-attribute 'annotation-edge-face nil :height 0.7)

(setq entity-highlights
      `((,(rx (group "[@")
	      (group (+? any))
	      (group "#" (+? any) "*]"))
	 (1 'annotation-edge-face)
	 (2 font-lock-function-name-face)
	 (3 'annotation-edge-face))
	(,(rx (group "[$")
	      (group (+? any))
	      (group "#" (+? any) "*]"))
	 (1 'annotation-edge-face)
	 (2 font-lock-string-face)
	 (3 'annotation-edge-face))))

(defvar annotator-mode-map nil "Keymap for `annotator-mode'")

(progn
  (setq annotator-mode-map (make-sparse-keymap))
  ;; ignore a-z, A-Z
  (dolist (char (number-sequence ?a ?z))
    (let ((lower (char-to-string char))
	  (upper (char-to-string (- char 32))))
      (define-key annotator-mode-map (kbd lower) 'ignore)
      (define-key annotator-mode-map (kbd upper) 'ignore)))
  ;; ignore 0-9
  (dolist (char (number-sequence ?0 ?9))
    (define-key annotator-mode-map
      (kbd (char-to-string char)) 'ignore))
  (define-key annotator-mode-map (kbd "q") 'ignore)
  (define-key annotator-mode-map (kbd "Q") 'ignore))

(defun adjust-point-after-click (event &optional _)
  "Adjust point.
Adjust point depending on which portion of the character the
cursor clicked on, if on the right half, move point after.
EVENT is the mouse event."
  (let* ((posn (event-end event))
         (x (car (posn-object-x-y posn)))
         (w (car (posn-object-width-height posn))))
    ;; ‘mouse-set-point’ is called twice when you click mouse, first
    ;; in ‘down-mouse-1’, called by ‘mouse-drag-region’ ->
    ;; ‘mouse-drag-track’ to set point, second in ‘mouse-1’, when
    ;; mouse released and Emacs realized that this is a click event.
    ;; We want to adjust point in both cases.
    (when (and (null (posn-object posn))
               (> x (/ w 2)))
      (forward-char))))

;; https://emacs-china.org/t/topic/13681/7
(define-minor-mode accurate-click-mode
  "Accurate point position on click."
  :global t
  :lighter ""
  (if accurate-click-mode
      (advice-add 'mouse-set-point :after #'adjust-point-after-click)
    (advice-remove 'mouse-set-point #'adjust-point-after-click)))

;;;###autoload
(define-derived-mode annotator-mode fundamental-mode "Annotator"
  "Major mode for Text Annotating"
  (buffer-face-set '(:height 1.3))
  (setq font-lock-defaults '(entity-highlights))
  (setq line-spacing 5)
  (setq cursor-type 'bar)
  (blink-cursor-mode 1)
  (accurate-click-mode 1))

(provide 'annotator)
;;; annotator.el ends here
