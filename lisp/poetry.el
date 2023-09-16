;;; poetry.el --- chinese peotry recommend           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  jingxiaobing

;; Author: jingxiaobing <jingxiaobing@jingxiaobingdeMacBook-Pro.local>
;; Keywords: tools, convenience, convenience, convenience, convenience

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

;; chinese poetry recommend

;;; Code:


(defvar poetry-cache-file "~/.emacs.d/.poem.json")
(defvar poetry-cache nil)

(defcustom my/poetry-local-folder nil
  "Local database, a folder of json files"
  :type 'string :group 'my)

(defun poetry-update-jinrishici ()
  "Download poem asynchronously from `jinrishici.com`"
  (let ((url-request-extra-headers
	     '(("X-User-Token" . "4xXGMG62BsykCCHWmj7ik4y2Z9bkiJ3T"))))
    (ignore-errors
      (url-retrieve
       "https://v2.jinrishici.com/sentence"
       (lambda (status)
	     (write-region url-http-end-of-headers (point-max) poetry-cache-file)
         (message "poem updated from jinrishici")
         (setq poetry-cache nil))))))

(defun poetry--rand-choice (items)
  "List random choice"
  (seq-elt items (random (length items))))

(defun poetry-update-local-database (folder)
  "Random choose from local database
json format ref: https://github.com/chinese-poetry/chinese-poetry"
  (let* ((files (directory-files folder nil "\\.json"))
         (file (file-name-concat folder (poetry--rand-choice files))))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((poetries (json-read))
             (poem (poetry--rand-choice poetries))
             (cache-poem `((data . ( ; 缓存统一格式
                                    (origin . ,poem)
                                    (content . ,(poetry--rand-choice (alist-get 'content poem)))
                                    )))))
        (setq poetry-cache nil)
        (message "poem updated from local db")
        (write-region (json-serialize cache-poem) nil poetry-cache-file)))))

(defun poetry-update ()
  "Update poetry"
  (if my/poetry-local-folder
      (poetry-update-local-database my/poetry-local-folder)
    (poetry-update-jinrishici)))

(defun poetry-get (prop)
  "Get poetry from cache file, PROP can be 'content, 'origin"
  (ignore-errors
    (if poetry-cache
        (alist-get prop poetry-cache)
      (with-temp-buffer
        (insert-file-contents poetry-cache-file)
        (let ((data (alist-get 'data (json-read))))
          (setq poetry-cache data)
          (alist-get prop data))))))

(defun poetry-get-formatted ()
  (let* ((poem (poetry-get 'origin))
         (lines (alist-get 'content poem))
         (content (mapconcat #'identity lines "\n")))
    (format "%s\n%s · %s\n%s"
            (alist-get 'title poem)
            (alist-get 'dynasty poem)
            (alist-get 'author poem)
            content)))

(provide 'poetry)
;;; poetry.el ends here
