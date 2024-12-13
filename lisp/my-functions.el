;;; my-functions.el --- my functions                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  jingxiaobing

;; Author: jingxiaobing <jingxiaobing@gmail.com>
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
(defun my/toggle-window-split ()
  (interactive)
  (when (= (count-windows) 2)
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

(require 'poetry)

(defun my/recentf-file-links ()
  "Recent files as `link` widget, maybe add nerd-icons."
  (mapcar
   (lambda (file)
     `(link :tag ,(if (featurep 'nerd-icons) (concat (nerd-icons-icon-for-file file) " " file) file)
            :format "%[%t\n%]"  ;; reference: (widget) Basic Types
            :button-face (:weight normal)
            :button-prefix ""
            :button-suffix ""
            :help-echo ,(concat "Open " file)
            :action recentf-open-files-action
            ,file))
   recentf-list))

(defun my/dashboard ()
  "My simple dashboard"
  (interactive)
  (recentf-dialog "*dashboard*"
    ;; recent files list
    (apply #'widget-create
           `(group
             :indent 2
             :format "\n%v\n"
             ,@(my/recentf-file-links)))
    (widget-insert " Startup in " (emacs-init-time "%.1f seconds, ")
                   (format "%d packages loaded\n\n" (length package-alist)))
    ;; poem
    (widget-insert (poetry-get-formatted) ?\n)
    (widget-move 1 t)
    (setq buffer-read-only t))
  (define-key recentf-dialog-mode-map "e" #'recentf-edit-list)
  (define-key recentf-dialog-mode-map "G"
              (lambda () (interactive) (poetry-update) (my/dashboard)))
  (define-key recentf-dialog-mode-map "g" #'my/dashboard))

(defun my/journal-capture ()
  "Mimic org-capture, and better than it.
Require package `edit-indirect'"
    (interactive)
    (setq my/journal-file (concat my/netdisk-dir (format-time-string "/Documents/%Y.org")))
    (with-current-buffer (find-file-noselect my/journal-file)
      (let ((mode-func major-mode)
            (heading (format-time-string "* [%Y-%m-%d %a]")))
        (goto-char (point-max))
        (if (re-search-backward (regexp-quote heading) nil t)
            (with-current-buffer (edit-indirect-region (point) (+ (length heading) (point)) t)
              (funcall mode-func)
              (insert "\n"))
          (or (bolp) (insert "\n"))
          (with-current-buffer (edit-indirect-region (1- (point-max)) (point-max) t)
            (funcall mode-func)
            (insert heading "\n"))))
      (add-hook 'edit-indirect-after-commit-functions (lambda (&rest _) (save-buffer)))))

;; z/zi like in zoxide, adapted from
;; https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
  "Navigate to a recent directory or subdirectory, in eshell."
  ;;todo: `regexp' might be expanded already (~ -> /home/my)
  (let ((dir (eshell-find-previous-directory regexp)))
    (cond
     ((member regexp '("~" "-" ".." "." "/")) (eshell/cd regexp))
     (dir (eshell/cd dir))  ;; z to recent dirs
     (t (eshell/cd regexp)))))  ;; maybe z to subdirs of pwd

(defun eshell/zi (&optional regexp)
  "Interactivly navigate to a recent directory in eshell, or to
any directory preferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     (regexp (eshell/z regexp))
     ((featurep 'consult-dir)
      (let* ((consult-dir--source-eshell
              `(:name "Eshell"
                      :narrow ?e
                      :category file
                      :face consult-file
                      :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (completing-read "cd: " eshell-dirs))))))

(defun my/coding-string (coding-system)
  "Get coding system string, for pretty print"
  (let ((s (symbol-name (coding-system-base coding-system))))
    (cond
     ((string-prefix-p "no-conversion" s) "BINARY")
     ((equal "undecided" s) "UTF-8")
     (t (upcase (replace-regexp-in-string "^\\(prefer-\\|chinese-\\)" "" s))))))

(provide 'my-functions)
;;; my-functions.el ends here
