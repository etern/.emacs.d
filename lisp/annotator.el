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

;;;###autoload
(define-derived-mode annotator-mode fundamental-mode "Annotator"
  "Major mode for Text Annotating"
  (buffer-face-set '(:height 1.3))
  (setq font-lock-defaults '(entity-highlights))
  (setq line-spacing 5)
  (setq cursor-type 'bar)
  (blink-cursor-mode 1))

(provide 'annotator)
;;; annotator.el ends here
