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

(defvar poem-file "~/.emacs.d/.poem.json")
(defvar poem-cache nil)

(defun poem-update ()
  "Download poem asynchronously from `jinrishici.com`"
  (let ((url-request-extra-headers
	 '(("X-User-Token" . "4xXGMG62BsykCCHWmj7ik4y2Z9bkiJ3T"))))
    (ignore-errors
      (url-retrieve
       "https://v2.jinrishici.com/sentence"
       (lambda (status)
	 (write-region url-http-end-of-headers (point-max) poem-file)
         (setq poem-cache nil))))))

(defun poem-get (prop)
  "Get poem from cache file, PROP can be 'content, 'origin"
  (ignore-errors
    (if poem-cache
        (alist-get prop poem-cache)
      (with-temp-buffer
        (insert-file-contents poem-file)
        (let ((data (alist-get 'data (json-read))))
          (setq poem-cache data)
          (alist-get prop data))))))

(defun poem-get-formatted ()
  (let* ((poem (poem-get 'origin))
         (lines (alist-get 'content poem))
         (content (mapconcat #'identity lines "\n")))
    (format "%s\n%s · %s\n%s"
            (alist-get 'title poem)
            (alist-get 'dynasty poem)
            (alist-get 'author poem)
            content)))

(defun my/bing-search (text)
  "use browser bing.com search keyword
keyword come from `active region` or `thing-at-point`"
  (interactive
   (if (use-region-p)
       (list (buffer-substring-no-properties (region-beginning) (region-end)))
     (list (thing-at-point 'symbol))))
  (if (null text)
      (message "don't know what to search")
    (browse-url (url-encode-url (format "https://bing.com/search?q=%s" text)))))

(defun my/toggle-debug (func)
  "toggle debug-on-entry"
  (interactive (list (symbol-at-point)))
  (if (advice-member-p #'debug--implement-debug-on-entry func)
      (progn (cancel-debug-on-entry func)
             (message "debug canceled"))
    (debug-on-entry func)
    (message "debug enabled")))

(defun my/copy-line-ref ()
  "Copy reference of current line as <filename>:<line-num>"
  (interactive)
  (let ((ref (format "%s:%d" (buffer-file-name) (line-number-at-pos))))
    (kill-new ref)
    (message (format "Copied: %s" ref))))

(defun my/dashboard ()
  "My simple dashboard"
  (interactive)
  (recentf-dialog "*my-dashboard*"
    ;; recent files list
    (apply #'widget-create
           `(group
             :indent 2
             :format "\n%v\n"
             ,@(recentf-open-files-items recentf-list)))
    ;; poem
    (widget-insert (poem-get-formatted))
    (widget-forward 1))
  (define-key recentf-dialog-mode-map "g"
    (lambda () (interactive) (my/dashboard) (poem-update))))

(provide 'my-functions)
;;; my-functions.el ends here
