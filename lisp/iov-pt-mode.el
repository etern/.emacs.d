;;; iov-pt-mode.el --- iov pattern mode              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  jingxiaobing

;; Author: jingxiaobing <jingxiaobing@jingxiaobingdeMacBook-Pro.local>
;; Keywords: languages

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

;; special mode at work, not intended for common use
;; (require 'iov-pt-mode)
;; (add-to-list 'auto-mode-alist

;;; Code:
(require 'xref)
(require 'thingatpt)

(defun xref-iov-pt-xref-backend ()
  "iov-pattern backend for xref."
  'iov-pt-mode)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql iov-pt-mode)))
  "xref后端接口：返回光标下的符号"
  (save-match-data
    (if (thing-at-point-looking-at "[a-zA-Z_0-9]+")
        (match-string-no-properties 0)
      nil)))

(cl-defmethod xref-backend-definitions ((_backend (eql iov-pt-mode)) symbol)
  "xref后端接口：根据输入符号，返回候选的xref-item"
  (mapcar
   (lambda (candidate)
     (with-current-buffer (find-file-noselect (iov-pt-mode--term-file))
       (xref-make (car candidate)
                  (xref-make-buffer-location (current-buffer)
                                             (cdr candidate)))))
   (xref-term-candidates symbol)))

(defun iov-pt-mode--term-file ()
  "Get term file name"
  (let* ((term-dir (file-name-directory (buffer-file-name)))
         (term-ext? (file-name-extension (buffer-file-name)))
         (term-ext (if term-ext? (concat "." term-ext?) nil)))
    (concat term-dir "term" term-ext)))

(defun xref-term-candidates (symbol)
  "Return list of labels matching SYMBOL."
  (with-current-buffer (find-file-noselect (iov-pt-mode--term-file))
    (goto-char (point-min))
    (let ((labels))
      ;; Match labels and constants
      (while (re-search-forward (format "\\[D:%s]" symbol) nil t)
        (push (cons (match-string-no-properties 0) (point))
              labels))
      labels)))

(setq iov-pt-keywords
      `(("#.*" . font-lock-comment-face)
        (,(rx (group (+ (in "A-Z_")))
	          (group (regexp "=>[0-9.]+=>")))
	     (1 font-lock-type-face)
	     (2 font-lock-string-face))
	    (,(rx (group (+ (in "a-zA-Z_0-9")))
	          (group "," digit ",")
	          (group (in "01") ";"))
	     (1 'default)
	     (2 font-lock-string-face)
	     (3 font-lock-constant-face))))

;;;###autoload
(define-derived-mode iov-pt-mode fundamental-mode "iov-pt"
  "Major mode for iov pattern"
  (setq font-lock-defaults '(iov-pt-keywords))
  (add-hook 'xref-backend-functions #'xref-iov-pt-xref-backend -100))


(provide 'iov-pt-mode)
;;; iov-pt-mode.el ends here
