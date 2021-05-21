;;; my-functions.el --- my functions                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  didi

;; Author: didi <didi@AILabs>
;; Keywords: abbrev

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
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;;; Code:

(define-skeleton my/snippet-dir-locals
  "Template of .dir-locals.el"
  nil
  "((nil . ((create-lockfiles . nil) ;; nil set variable for all mode
         (eval . (message \"executed\")))) ;; `eval` pseudo-variable for evaluation
 (c-mode . ((fill-column . 50))))")

(defun decode-utf8-string (str)
  (decode-coding-string
   (mapconcat #'unibyte-string (seq-into str 'list) "") 'utf-8))

(defun get-poem-then-update (&optional new-line)
  "Schedule async download poem, read poem from cache file and
return poem string"
  (let ((poem-cache-file "~/.emacs.d/.poem.txt")
	(url-request-extra-headers
	 '(("X-User-Token" . "4xXGMG62BsykCCHWmj7ik4y2Z9bkiJ3T"))))
    (ignore-errors
      (url-retrieve
       "https://v2.jinrishici.com/sentence"
       (lambda (status) ;; Schedule async download
	 (goto-char url-http-end-of-headers)
	 (let* ((data (alist-get 'data (json-read)))
		(content (alist-get 'content data)))
	   (with-temp-file poem-cache-file
	     (insert (decode-utf8-string content))
	     (when new-line
	       (insert "\n"))))))
      (with-temp-buffer ;; read cache immediately
	(insert-file-contents poem-cache-file)
	(buffer-string)))))

(provide 'my-functions)
;;; my-functions.el ends here
