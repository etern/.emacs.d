;;; wget.el --- simple wget                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  

;; Author:  <bjjingxiaobing@BIH-L-2664>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; wget for eshell

;;; Code:

(defun wget--can-write-p (filename)
  "Check if file already exists, ask user what to do"
  (if (file-exists-p filename)
      (if (yes-or-no-p (concat "File '" filename "' already exists, overwrite?")) t nil)
    t))

(defun wget-save-file (status filename)
  "Callback for url-retrieve, save buffer to file"
  (when (wget--can-write-p filename)
    (write-region (1+ url-http-end-of-headers) (point-max) filename)
    (message "Save to file: %s%s" default-directory filename)))

(defun wget--parse-url (url)
  "Parse url string to url object, if there are no url scheme,
default to http"
  (if (null (string-match "^http" url))
      (setq url (concat "http://" url)))
  (url-generic-parse-url url)
  )

(defun wget--make-file-name (url-obj)
  "make file name from url"
  (let ((path (file-name-nondirectory (url-filename url-obj))))
    (if (string= "" path) (setq path "index.html"))
    path
  ))

;;;###autoload
(defun wget (url)
  (let* ((url-obj (wget--parse-url url))
         (filename (wget--make-file-name url-obj)))
    (url-retrieve url-obj 'wget-save-file (list filename)) nil))

(provide 'wget)
;;; wget.el ends here

